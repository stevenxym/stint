void main()
{
	string s1 = "cabc";
	string s2 = "c";

	string s3 = s1 - s2;
	string s4 = s1 - s2 @ 1;
	
	<< std s3;
	<< std "\n";
	<< std s4

	return;
}