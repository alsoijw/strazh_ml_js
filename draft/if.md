```js
for(let j = 1; j < 6; j += 2) {
  _ = [];
  for(let i = 0; i < 6; i++) {
    _.push({ i: i, f: i == j});
  }
  a = _[0]
  if((a = _[1]).f) {
    a = _[2]
  } else if(_[3].f) {
    a = _[4]
  } else {
    a = _[5];
  }
  console.log(a.i)
}
```
