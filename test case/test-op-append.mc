void main()
{
	string s1 = "ab";
	string s2 = "cd";
	string s3 = s1 + s2;
	string s4 = s1 + s2 @ 0;
	<< std s3;
	<< std "\n";
	<< std s4;

	return;
}