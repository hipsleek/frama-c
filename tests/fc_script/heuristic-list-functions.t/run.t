  $ frama-c-script heuristic-list-functions true true *.c *.i
  build-callgraph.c:2:4: main (definition)
  build-callgraph.c:11:11: fn2 (declaration)
  build-callgraph.c:13:18: fn1 (definition)
  build-callgraph.c:21:27: main1 (definition)
  build-callgraph.c:29:31: main2 (definition)
  build-callgraph.c:41:44: main3 (definition)
  build-callgraph.c:47:58: main4 (definition)
  build-callgraph.c:59:64: main (definition)
  find-fun.c:2:3: main2 (declaration)
  find-fun.c:9:13: main3 (definition)
  find-fun2.c:1:3: main3 (declaration)
  find-fun2.c:6:8: f (definition)
  find-fun2.c:10:13: g (definition)
  find-fun2.c:15:17: h (definition)
  find-fun2.c:24:27: static_fun (definition)
  list-functions.c:4:11: static_fun (definition)
  list-functions.c:13:18: k (definition)
  main.c:1:3: main (definition)
  main2.c:1:3: fake_main (definition)
  main2.c:5:7: domain (definition)
  main2.c:9:11: main2 (definition)
  main3.c:1:4: main (definition)
  make-wrapper.c:1:1: defined (declaration)
  make-wrapper.c:3:3: specified (declaration)
  make-wrapper.c:5:5: external (declaration)
  make-wrapper.c:7:9: large_name_to_force_line_break_in_stack_msg (definition)
  make-wrapper.c:11:13: rec (definition)
  make-wrapper.c:15:21: main (definition)
  make-wrapper2.c:1:3: defined (definition)
  make-wrapper2.c:9:9: specified (declaration)
  make-wrapper2.c:12:12: external (declaration)
  make-wrapper3.c:3:5: external (definition)
  heuristic-detect-recursion.i:3:5: g (definition)
  heuristic-detect-recursion.i:7:10: f (definition)
  heuristic-detect-recursion.i:12:15: h (definition)
  heuristic-detect-recursion.i:17:19: i (definition)
  heuristic-detect-recursion.i:21:23: j (definition)
  heuristic-detect-recursion.i:25:25: l (declaration)
  heuristic-detect-recursion.i:26:26: m (declaration)
  heuristic-detect-recursion.i:28:30: k (definition)
  heuristic-detect-recursion.i:32:34: l (definition)
  heuristic-detect-recursion.i:36:38: m (definition)
  heuristic-detect-recursion.i:40:41: norec (definition)
  heuristic-detect-recursion.i:43:45: direct_rec (definition)
  heuristic-detect-recursion.i:47:47: indirect_rec1 (declaration)
  heuristic-detect-recursion.i:49:51: indirect_rec2 (definition)
  heuristic-detect-recursion.i:53:55: indirect_rec1 (definition)
  heuristic-detect-recursion.i:57:57: decl_only (declaration)
  heuristic-detect-recursion.i:59:59: one_liner_function (definition)
  heuristic-detect-recursion.i:61:61: multiple_indirect1 (declaration)
  heuristic-detect-recursion.i:63:66: multiple_indirect2 (definition)
  heuristic-detect-recursion.i:68:71: multiple_indirect1 (definition)