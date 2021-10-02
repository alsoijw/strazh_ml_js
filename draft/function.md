```js
i = 0;
function _() {
	return i++;
}

cv = 0;
cl = 0;
co = `var_a = _();

function t1() {
	u(var_a);
	var_a = _();
	t2();
	u(var_a);
	var_a = _();
}

function t2() {
	s(var_a);
	var_a = _();
}
`
	.replace(/^(.*var_a.*)$/gm, '$1 -~-')
	.split("-~-")
	.reduce((a, i, j) => (cl++, `${a}log_f(${j - 1}, var_a) ${i}`))

c = co
	.split("_()")
	.reduce((a, i, j) => (cv++, `${a}_[${j - 1}]${i}`))

_ = [];
for(let i = 0; i < cv; i++) {
	_.push({ i: i });
}

function ord(c) {
	let b = c;
	
	function rec(a, o = []) {
		if(a.length) {
			return a.map((i, j, a) => {
				if(a.length) {
					var a = [...a];
					a.splice(j, 1);
					let n = [...o];
					n.push(i);
					return rec(a, n);
				}
			});
		}
		return o;
	}

	return rec(b).flat(b.length - 1)
}

function log_f(i, v) {
	log_a[i].push(v.i);
}

log_i = () => new Array(cl).fill(0).map(_ => [])

function s(v) {} function u(v) {}

console.log(
	ord(
		[ ...c.matchAll(/(?<= )t[0-9]/g) ]
		.map(i => i[0] + '()')
	).map(i => {
		log_a = log_i();

		lcc = i.join(";");
		eval(
		`${c}

		${lcc}; ${lcc}`
		)

		return log_a;
	}).reduce(
		(a, i) => i.map((v, k) => [ ...v, ...a[k] ]),
		log_i()
	).map(i => [ ...new Set(i)])
	.reduce(
		(a, i, j) => a.replace(`log_f(${j}, var_a) `, `// ${i.join(' | ')}`),
		co
	)
)
```
