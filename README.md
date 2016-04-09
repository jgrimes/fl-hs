# fl-hs

Work in progress -- only a few things work and the code ain't great.

    λ> eval "map:+:<<99,99>,<1,2,3,4,5>>"
    (D <Seq: [<Atom: Number 198 >,<Atom: Number 15 >] >,[])
    
    λ> eval "comp:<+,map:~2>:<1,2,3,4,5>"
    (D <Atom: Number 10 >,[])
    
    λ> eval "[+,*,~2,K:1]:<3,5,6>"
    (D <Seq: [<Atom: Number 14 >,<Atom: Number 90 >,<Atom: Number 2 >,<Atom: Number 1 >] >,[])
    
    λ> eval "cons:<+,*>:<3,5,6>"
    (D <Seq: [<Atom: Number 14 >,<Atom: Number 90 >] >,[])

    λ> repl
    FL> def length == comp:<+,map:~1>
    "Defined."
    FL> length:<1,2,3>
    (D <Atom: Number 3 >,[])
    FL> map:length:<<1,2,3,4,5>,<53,67,12,4>,<6>>
    (D <Seq: [<Atom: Number 5 >,<Atom: Number 4 >,<Atom: Number 1 >] >,[])
