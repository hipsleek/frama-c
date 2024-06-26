/* run.config*
 PLUGIN: @EVA_MAIN_PLUGINS@
   OPT: -no-annot -eva @EVA_CONFIG@ -then -kernel-warn-key=annot-error=active -annot -eva
   OPT: -kernel-warn-key=annot-error=active -eva @EVA_CONFIG@ -main main3
   OPT: -kernel-warn-key=annot-error=active -eva @EVA_CONFIG@ -main main_err1
   OPT: -kernel-warn-key=annot-error=active -eva @EVA_CONFIG@ -main main_err2
*/

void main(void)
{ int n = 13;
  int i,j;
  /*@ loop widen_hints i, 12, 13; */
  for (i=0; i<n; i++)
    {
      j = 4 * i + 7;
    }
}


void main_err1(void)
{ int n = 13;
  int i,j;
  /*@ loop widen_hints "all", 12 ; */
  for (i=0; i<n; i++)
    {
      j = 4 * i + 7;
    }
}



void main_err2(void)
{ int n = 13;
  int i,j;
  for (i=0; i<n; i++)
    {
      j = 4 * i + 7;
    }
}


void main_unhelpful () {
  int max = 25;
  int next = 0;
  int i;

/*@ loop widen_hints next, 24; */ // This hint is unhelpful, but used to interfere with the bound for i.
  for (i=0;i<30;i++) {
    int vsize = max;
    int vnext = next;

    if(vsize > vnext)
      next++;
  }
}

void main_multiple_hints () {
  int maxj = 17;
  int maxk = 11;
  int j = 0;
  int k = 0;

  //@ loop widen_hints j, 17; loop widen_hints k, 11;
  // 18 and 12 are actually better bounds in this case (one less iteration)
  for (int i=0; i<10; i++) {

    Frama_C_show_each(i, j, k);

    if (j <= maxj) {
      j++;
    }
    if (k <= maxk) {
      k++;
    }

  }
}

void main3() {
  main_unhelpful ();
  main_multiple_hints ();
}
