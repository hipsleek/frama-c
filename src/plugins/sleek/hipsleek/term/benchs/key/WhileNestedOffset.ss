void increase (int i)
case {
	i>=10 -> requires Term ensures true;
	i<10 -> requires Loop ensures false;
}
{
	int j;
	loop_1 (i, j);
}

void loop_1 (ref int i, ref int j)
case {
	i>=10 -> requires Term ensures i'=i & j'=j;
	i<10 -> requires Loop ensures false;
}
{
	if (i < 10) {
		j = i;
		loop_2 (i, j);
		i++;
		loop_1 (i, j);
	}
}

void loop_2 (ref int i, ref int j)
case {
	j>5 -> requires Loop ensures false;
	j<=5 -> requires Term ensures i'=i & j'=j;
}
{
	if (j > 5) {
		j++;
		loop_2 (i, j);
	}
}
