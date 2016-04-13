# fl-hs

Work in progress -- only a few things work and the code ain't great.

    λ> repl
    FL> def length == comp:<+,map:~1>
    "Defined."
    FL> length:<1,2,3>
    (D <Atom: Number 3 >,[])
    FL> map:length:<<1,2,3,4,5>,<53,67,12,4>,<6>>
    (D <Seq: [<Atom: Number 5 >,<Atom: Number 4 >,<Atom: Number 1 >] >,[])

Now with recursive definitions...

    λ> repl
    FL> def eq0 == comp:<=,[id,~0]>
    "Defined."
    FL> def sub1 == C:+:-1
    "Defined."
    FL> def fact == eq0 -> ~1; comp:<*,[id,comp:<fact,sub1>]>
    "Defined."
    FL> fact:5
    (D <Atom: Number 120 >,[])
    FL> fact:3
    (D <Atom: Number 6 >,[])
    FL> fact:50
    (D <Atom: Number 30414093201713378043612608166064768844377641568960512000000000000 >,[])
