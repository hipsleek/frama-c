[kernel] Parsing sparecode.i (no preprocessing)
[slicing] slicing requests in progress...
[eva] Analyzing a complete application starting at main
[eva] Computing initial state
[eva] Initial state computed
[eva:initial-state] Values of globals at initialization
  G ∈ {0}
[eva] computing for function f <- main.
  Called from sparecode.i:18.
[eva] Recording results for f
[eva] Done for function f
[eva] computing for function f <- main.
  Called from sparecode.i:19.
[eva] Recording results for f
[eva] Done for function f
[eva] computing for function f <- main.
  Called from sparecode.i:20.
[eva] Recording results for f
[eva] Done for function f
[eva] Recording results for main
[eva] Done for function main
[eva:summary] ====== ANALYSIS SUMMARY ======
  ----------------------------------------------------------------------------
  2 functions analyzed (out of 2): 100% coverage.
  In these functions, 9 statements reached (out of 9): 100% coverage.
  ----------------------------------------------------------------------------
  No errors or warnings raised during the analysis.
  ----------------------------------------------------------------------------
  0 alarms generated by the analysis.
  ----------------------------------------------------------------------------
  No logical properties have been reached by the analysis.
  ----------------------------------------------------------------------------
[slicing] initializing slicing ...
[slicing] interpreting slicing requests from the command line...
[pdg] computing for function main
[from] Computing for function f
[from] Done for function f
[pdg] done for function main
[slicing] applying all slicing requests...
[slicing] applying 0 actions...
[slicing] applying all slicing requests...
[slicing] applying 1 actions...
[slicing] applying actions: 1/1...
[pdg] computing for function f
[pdg] done for function f
[slicing] exporting project to 'Slicing export'...
[slicing] applying all slicing requests...
[slicing] applying 0 actions...
[sparecode] remove unused global declarations from project 'Slicing export tmp'
[sparecode] removed unused global declarations in new project 'Slicing export'
/* Generated by Frama-C */
int G;
int f_slice_1(int x, int y)
{
  G = y;
  return x;
}

int main(void)
{
  int a = 1;
  int b = 1;
  f_slice_1(a,b);
  a = f_slice_1(G + 1,b);
  return a;
}


