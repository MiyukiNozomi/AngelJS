# AngelJS

this is a sort of javascript but, slitghtly more akward if you want to 
call it that;

Actually, its more of a typescript without being null-safe, 

lets show a pratical example:

in  javascript, you could easily do something like this:

```js
let a = new vec3();

a = "aaa";

console.log(a.x); // undefined!
```

in AngelJS, you could never dream of assigning a vector to a string,
if you were to try something like this here, you would get a beautiful 

Error in line 3 > Cannot assign a 'String' to a 'struct#vec3'

by the way, currently AngelJS doesn't offer functions and modules for now 
as i'm still trying to figure out a descent way to implement thoose.

keep in mind that this project was made not to be a full language, but yet 
to be used in the games i work on.

# TODOs

[  ] Add Annotations (for use in other projects)