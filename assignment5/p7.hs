f 0 0=0
f 0 y=g 0 y
f x y=3 + (g (x-1) (y+1))

g 0 0=0
g x 0=f x 0
g x y=2+(f x (y-1))

res 0 0=0
res 0 _=0
res a 0=a
res a b=res (a-1) (b-1)

ge 0 0=1
ge 0 _=0
ge a 0=1
ge a b=ge (a-1) (b-1)

fact 0=0
fact 1=1
fact x=x*fact(x-1)

myIf 0 _ a=a
myIf _ a _=a

mdl 0 0=0
mdl _ 0=0
mdl 0 _=0
mdl a b=myIf (ge a b) (mdl (res a b) b) a

gamma 0 =0
gamma a =myIf (ge a 2) ((gamma (res a 2))+1) 0