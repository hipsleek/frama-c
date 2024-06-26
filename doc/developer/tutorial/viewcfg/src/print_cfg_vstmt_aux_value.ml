method! vstmt_aux s =
  let color =
    if Eva.Analysis.is_computed () then
      let reachable = Eva.Results.is_reachable s in
      if reachable then "fillcolor=\"#ccffcc\" style=filled"
      else "fillcolor=pink style=filled"
    else ""
  in
  Format.fprintf out "@[s%d@ [label=%S %s]@];@ "
    s.sid (Pretty_utils.to_string print_stmt s.skind) color;
  List.iter
    (fun succ -> Format.fprintf out "@[s%d -> s%d;@]@ " s.sid succ.sid)
    s.succs;
  Cil.DoChildren
