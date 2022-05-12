type 'a visitor = {
  expression2loc : (Loc.t, Loc.t) Flow_ast.Expression.t -> (Loc.t, Loc.t) Flow_ast.Expression.t' -> 'a;
  pattern_tail : bool
}

let ast_walk visitor code =
  let rec array_pattern2loc = function
    | Flow_ast.Pattern.Array.Element (_, a) -> pattern2loc a.argument @ expression2loc_opt a.default
    | Flow_ast.Pattern.Array.RestElement (_, a) -> pattern2loc a.argument
    | Flow_ast.Pattern.Array.Hole _ -> []
  and pattern2loc i = match snd i with
    | Flow_ast.Pattern.Expression e ->
      (if visitor.pattern_tail then expression2loc_tail else expression2loc) e
    | Flow_ast.Pattern.Identifier _ -> []
    | Flow_ast.Pattern.Array a -> List.map array_pattern2loc a.elements |> List.flatten
    | Flow_ast.Pattern.Object a -> List.map (fun i -> match i with
        | Flow_ast.Pattern.Object.Property (_, b) ->
          (match b.key with
           | Flow_ast.Pattern.Object.Property.Literal _ -> []
           | Flow_ast.Pattern.Object.Property.Identifier _ -> []
           | Flow_ast.Pattern.Object.Property.Computed (_, b) -> expression2loc b.expression) @
          pattern2loc b.pattern @ expression2loc_opt b.default
        | Flow_ast.Pattern.Object.RestElement (_, b) -> pattern2loc b.argument
      ) a.properties |> List.flatten
  and expression2loc i =
    let this_val = snd i |> visitor.expression2loc i in
    match snd i with
    | Flow_ast.Expression.Array a -> List.map (function
        | Flow_ast.Expression.Array.Expression b -> expression2loc b
        | Flow_ast.Expression.Array.Spread (_, b) -> let open Flow_ast.Expression.SpreadElement in
          expression2loc b.argument
        | Flow_ast.Expression.Array.Hole _ -> []
      ) a.elements |> List.flatten
    | Flow_ast.Expression.ArrowFunction a -> function2loc a
    | Flow_ast.Expression.Assignment d -> this_val :: pattern2loc d.left @ expression2loc d.right
    | Flow_ast.Expression.Binary a -> expression2loc a.left @ expression2loc a.right
    | Flow_ast.Expression.Call a -> expression2loc a.callee @ expression_arg_list2loc a.arguments
    | Flow_ast.Expression.Class a -> class2loc a
    | Flow_ast.Expression.Comprehension a ->
      (List.map expression_comprehension_block2loc a.blocks |> List.flatten) @
      expression2loc_opt a.filter
    | Flow_ast.Expression.Conditional a -> expression2loc a.test @ expression2loc a.consequent @ expression2loc a.alternate
    | Flow_ast.Expression.Function a -> function2loc a
    | Flow_ast.Expression.Generator a ->
      (List.map expression_comprehension_block2loc a.blocks |> List.flatten) @
      (match a.filter with
       | Some v -> expression2loc v
       | None -> [])
    | Flow_ast.Expression.Identifier _ -> [this_val]
    | Flow_ast.Expression.Import a -> expression2loc a.argument
    (* FIXME *)
    | Flow_ast.Expression.JSXElement _ -> assert false
    (* FIXME *)
    | Flow_ast.Expression.JSXFragment _ -> assert false
    | Flow_ast.Expression.Literal _ -> []
    | Flow_ast.Expression.Logical a -> expression2loc a.left @ expression2loc a.right
    | Flow_ast.Expression.Member d -> expression_member2loc d this_val
    | Flow_ast.Expression.MetaProperty _ -> []
    | Flow_ast.Expression.New a ->
      (match a.arguments with
       | Some (_, b) -> List.map expression_or_spread2loc b.arguments |> List.flatten
       | None -> []) @
      expression2loc a.callee
    | Flow_ast.Expression.Object a ->
      let property2loc = function
        | Flow_ast.Expression.Object.Property.Init c -> expression_object_property2loc c.key @ expression2loc c.value
        | Flow_ast.Expression.Object.Property.Method c -> expression_object_property2loc c.key @ function2loc (snd c.value)
        | Flow_ast.Expression.Object.Property.Get c -> expression_object_property2loc c.key @ function2loc (snd c.value)
        | Flow_ast.Expression.Object.Property.Set c -> expression_object_property2loc c.key @ function2loc (snd c.value)
      in
      List.map (function
          | Flow_ast.Expression.Object.Property (_, a) -> property2loc a
          | Flow_ast.Expression.Object.SpreadProperty (_, a) -> expression2loc a.argument
        ) a.properties |> List.flatten
    | Flow_ast.Expression.OptionalCall a -> expression_call2loc a.call
    | Flow_ast.Expression.OptionalMember a -> expression_member2loc a.member this_val
    | Flow_ast.Expression.Sequence a -> List.map expression2loc a.expressions |> List.flatten
    | Flow_ast.Expression.Super _ -> []
    | Flow_ast.Expression.TaggedTemplate a -> expression2loc a.tag @ expression_template_literal2loc (snd a.quasi)
    | Flow_ast.Expression.TemplateLiteral a -> expression_template_literal2loc a
    | Flow_ast.Expression.This _ -> [this_val]
    | Flow_ast.Expression.TypeCast a -> expression2loc a.expression
    | Flow_ast.Expression.Unary b -> (match b.operator with
        | Flow_ast.Expression.Unary.Delete -> expression2loc_tail b.argument
        | _ -> expression2loc b.argument)
    | Flow_ast.Expression.Update b -> expression2loc_tail b.argument
    | Flow_ast.Expression.Yield a -> expression2loc_opt a.argument
  and expression2loc_tail i = match expression2loc i with
    | [] -> []
    | _ :: tl -> tl
  and expression2loc_opt = function
    | Some b -> expression2loc b
    | None -> []
  and function2loc a =
    (List.map (fun (_, i) ->
         let open Flow_ast.Function.Param in
         expression2loc_opt i.default
       ) (snd a.params).params |> List.flatten) @
    match a.body with
    | Flow_ast.Function.BodyBlock (_, b) -> List.map statement2loc b.body |> List.flatten
    | Flow_ast.Function.BodyExpression b -> expression2loc b
  and statement2loc (_, i) = match i with
    | Flow_ast.Statement.Block a -> List.map statement2loc a.body |> List.flatten
    | Flow_ast.Statement.Break _ -> []
    | Flow_ast.Statement.ClassDeclaration a -> class2loc a
    | Flow_ast.Statement.Continue _ -> []
    | Flow_ast.Statement.Debugger _ -> []
    | Flow_ast.Statement.DeclareClass _ -> []
    | Flow_ast.Statement.DeclareExportDeclaration _ -> []
    | Flow_ast.Statement.DeclareFunction _ -> []
    | Flow_ast.Statement.DeclareInterface _ -> []
    | Flow_ast.Statement.DeclareModule a -> block2loc a.body
    | Flow_ast.Statement.DeclareModuleExports _ -> []
    | Flow_ast.Statement.DeclareTypeAlias _ -> []
    | Flow_ast.Statement.DeclareOpaqueType _ -> []
    | Flow_ast.Statement.DeclareVariable _ -> []
    | Flow_ast.Statement.DoWhile a -> statement2loc a.body @ expression2loc a.test
    | Flow_ast.Statement.Empty _ -> []
    | Flow_ast.Statement.EnumDeclaration _ -> []
    | Flow_ast.Statement.ExportDefaultDeclaration a -> (match a.declaration with
        | Flow_ast.Statement.ExportDefaultDeclaration.Declaration b -> statement2loc b
        | Flow_ast.Statement.ExportDefaultDeclaration.Expression b -> expression2loc b)
    | Flow_ast.Statement.ExportNamedDeclaration a -> (match a.declaration with
        | Some b -> statement2loc b
        | None -> [])
    | Flow_ast.Statement.Expression c -> expression2loc c.expression
    | Flow_ast.Statement.For a ->
      (match a.init with
       | Some b -> (match b with
           | Flow_ast.Statement.For.InitDeclaration (_, a) -> variabledeclaration2loc a
           | Flow_ast.Statement.For.InitExpression a -> expression2loc a)
       | None -> []) @
      expression2loc_opt a.test @
      expression2loc_opt a.update @
      statement2loc a.body
    | Flow_ast.Statement.ForIn a ->
      (match a.left with
       | Flow_ast.Statement.ForIn.LeftDeclaration (_, a) -> variabledeclaration2loc a
       | Flow_ast.Statement.ForIn.LeftPattern a -> pattern2loc a) @
      expression2loc a.right @
      statement2loc a.body
    | Flow_ast.Statement.ForOf a -> (match a.left with
        | Flow_ast.Statement.ForOf.LeftDeclaration (_, a) -> variabledeclaration2loc a
        | Flow_ast.Statement.ForOf.LeftPattern a -> pattern2loc a)
    | Flow_ast.Statement.FunctionDeclaration a -> function2loc a
    | Flow_ast.Statement.If a ->
      expression2loc a.test @ statement2loc a.consequent @ (match a.alternate with
          | Some (_, v) -> statement2loc v.body
          | None -> [])
    | Flow_ast.Statement.ImportDeclaration _ -> []
    | Flow_ast.Statement.InterfaceDeclaration _ -> []
    | Flow_ast.Statement.Labeled a -> statement2loc a.body
    | Flow_ast.Statement.Return a -> expression2loc_opt a.argument
    | Flow_ast.Statement.Switch a ->
      expression2loc a.discriminant @
      (List.map (fun (_, i) ->
           let open Flow_ast.Statement.Switch.Case in
           expression2loc_opt i.test @
           (List.map statement2loc i.consequent |> List.flatten)
         ) a.cases |> List.flatten)
    | Flow_ast.Statement.Throw a -> expression2loc a.argument
    | Flow_ast.Statement.Try a ->
      block2loc a.block  @
      (match a.handler with
       | Some (_, b) -> (match b.param with
           | Some c -> pattern2loc c
           | None -> []) @ block2loc b.body
       | None -> []) @
      block2loc_opt a.finalizer
    | Flow_ast.Statement.TypeAlias _ -> []
    | Flow_ast.Statement.OpaqueType _ -> []
    | Flow_ast.Statement.VariableDeclaration a -> variabledeclaration2loc a
    | Flow_ast.Statement.While a -> expression2loc a.test @ statement2loc a.body
    | Flow_ast.Statement.With a -> expression2loc a._object @ statement2loc a.body
  and expression_arg_list2loc (_, a) = List.map expression_or_spread2loc a.arguments |> List.flatten
  and expression_or_spread2loc = function
    | Flow_ast.Expression.Expression a -> expression2loc a
    | Flow_ast.Expression.Spread (_, a) -> expression2loc a.argument
  and block2loc (_, a) = List.map statement2loc a.body |> List.flatten
  and block2loc_opt a = match a with
    | Some b -> block2loc b
    | None -> []
  and variabledeclaration2loc a = List.map (fun (_, i) ->
      let open Flow_ast.Statement.VariableDeclaration.Declarator in
      pattern2loc i.id @ expression2loc_opt i.init
    ) a.declarations |> List.flatten
  and expression_template_literal2loc a = List.map expression2loc a.expressions |> List.flatten
  and expression_call2loc a = expression2loc a.callee @ expression_arg_list2loc a.arguments
  and expression_member2loc d this_val = this_val :: expression2loc d._object @ (match d.property with
      | Flow_ast.Expression.Member.PropertyIdentifier _ -> []
      | Flow_ast.Expression.Member.PropertyPrivateName _ -> []
      | Flow_ast.Expression.Member.PropertyExpression f ->
        expression2loc f)
  and expression_comprehension_block2loc (_, a) =
    let open Flow_ast.Expression.Comprehension.Block in
    pattern2loc a.left @ expression2loc a.right
  and expression_object_property2loc = function
    | Flow_ast.Expression.Object.Property.Literal _ -> []
    | Flow_ast.Expression.Object.Property.Identifier _ -> []
    | Flow_ast.Expression.Object.Property.PrivateName _ -> []
    | Flow_ast.Expression.Object.Property.Computed (_, b) -> expression2loc b.expression
  and class_property_value2loc = function
    | Flow_ast.Class.Property.Declared -> []
    | Flow_ast.Class.Property.Uninitialized -> []
    | Flow_ast.Class.Property.Initialized c -> expression2loc c
  and class2loc a =
    List.map (function
        | Flow_ast.Class.Body.Method (_, b) ->
          (expression_object_property2loc b.key @
           function2loc @@ snd b.value) @
          (List.map (fun (_, i) ->
               let open Flow_ast.Class.Decorator in
               expression2loc i.expression
             ) b.decorators |> List.flatten)
          |> List.flatten
        | Flow_ast.Class.Body.Property (_, b) ->
          (expression_object_property2loc b.key @
           class_property_value2loc b.value)
          |> List.flatten
        | Flow_ast.Class.Body.PrivateField (_, b) -> class_property_value2loc b.value |> List.flatten
      ) (snd a.body).body
  in statement2loc code
