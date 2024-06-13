void loop(ref int x)
 case {
	x>11 ->
		requires x>=0 // bounded variance
    variance [0,1,x]
    ensures "lp":x'=10;
	x<=11 -> 
		variance [0,0]
    ensures "base":x'=x-1;
 }

{ 
	int orig_x=x;
	x=x-1;
	if (x>10) {
		//assert x>10 & orig_x-x>0;
		//assume false;
    //assert "lp": x'>=0;
    assert "lp":orig_x'-x'>0;
    assert "base":0-0>0;
    loop(x);
    //assume x'=10;
		return;
  }
	else return;
}

