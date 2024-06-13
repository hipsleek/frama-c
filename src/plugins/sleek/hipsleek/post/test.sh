EX="./test_ex.sh ../hip"
mkdir -p ref/bugs/
mkdir -p result/bugs/
mkdir -p ref/field/
mkdir -p result/field/
mkdir -p ref/flow/
mkdir -p result/flow/
mkdir -p ref/t/
mkdir -p result/t/
$EX bugs/ack1.ss
$EX bugs/ll-append1.ss
$EX bugs/ll-data.ss
$EX bugs/ll-length1.ss
$EX bugs/ll-length2.ss
$EX bugs/ll-length3.ss
$EX bugs/mut-rec1.ss
$EX bugs/mut-rec2.ss
$EX bugs/tree-data1.ss
$EX bugs/tree-data.ss
$EX bugs/tree-height1.ss
$EX bugs/tree-size1.ss
$EX flow/brk1b.ss
$EX flow/brk1c.ss
$EX flow/brk1.ss
$EX flow/flow2a.ss
$EX flow/flow2.ss
$EX flow/flow3.ss
$EX t/ack2.ss --infer-lex
$EX t/ack3a.ss --infer-lex
$EX t/ack3b.ss --infer-lex
$EX t/ack3c.ss --infer-lex
$EX t/ack3.ss --infer-lex
$EX t/ack-5.ss --infer-lex
$EX t/ack.ss --infer-lex
$EX t/ack-z.ss --infer-lex
$EX t/deadpool.ss
$EX t/exc1.ss
$EX t/exc3a.ss
$EX t/exc3.ss
$EX t/exc4.ss
$EX t/exc5.ss
$EX t/exc6a.ss
$EX t/exc6b.ss
$EX t/exc6c.ss
$EX t/exc6d.ss
$EX t/exc6e.ss
$EX t/exc6f.ss
$EX t/exc6.ss
$EX t/exc7a.ss
$EX t/exc7b.ss
$EX t/exc7.ss
$EX t/exc8.ss
$EX t/exc.ss
$EX t/f91-a.ss
$EX t/f91-b.ss
$EX t/f91-only-rec.ss
$EX t/f91-rec.ss
$EX t/f91-z.ss
$EX t/fact1a.ss
$EX t/fact1.ss
$EX t/fact2.ss
$EX t/fact-case2.ss
$EX t/fact.ss
$EX t/foo-fact1.ss
$EX t/foo-fact2.ss
$EX t/foo-fact.ss
$EX t/len1.ss
$EX t/len2.ss
$EX t/len.ss
$EX t/ll-app-post1.ss
$EX t/ll-app-post.ss
$EX t/ll-insert2.ss
$EX t/ll-insert.ss
$EX t/ll.ss
$EX t/loop1.ss
$EX t/loop1v.ss
$EX t/loop3.ss
$EX t/loop-f91.ss
$EX t/loop.ss
$EX t/multi-join2.ss
$EX t/mut-rec1.ss
$EX t/mut-rec2.ss
$EX t/non-rec1.ss
$EX t/non-rec2.ss
$EX t/post1.ss
$EX t/post-2a.ss
$EX t/post-2b.ss
$EX t/post-2.ss
$EX t/rb.ss
$EX t/rec-f91a1.ss
$EX t/rec-f91a2.ss
$EX t/rec-f91a3.ss
$EX t/rec-f91a.ss
$EX t/rec-f91b.ss
$EX t/rec-f91c.ss
$EX t/rec-f91d.ss
$EX t/rec-f91e.ss
$EX t/rec-f91f.ss
$EX t/rec-f91g.ss
$EX t/rec-f91.ss
$EX t/rec-fgen0.ss
$EX t/rec-fgen1.ss
$EX t/rec-fgen2.ss
$EX t/rec-fgen3.ss
$EX t/rec-fgen.ss
$EX t/sc-1.ss
$EX t/shape.ss
$EX t/strong1.ss
$EX t/strong2.ss
$EX t/tree-data.ss
$EX t/tree-height.ss
$EX t/tree-size.ss
$EX t/wloop1.ss
$EX t/wloop2.ss
$EX t/wloop.ss
