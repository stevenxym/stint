int fib (int x)
{
	if (x < 2){
		return 1;
	}
	else{ 
		return fib(x-1) + fib(x-2);
	}
}

void main()
{
	<< std fib(0);
	<< std "\n";
	<< std fib(1);
	<< std "\n";
	<< std fib(2);
	<< std "\n";
	<< std fib(3);
	<< std "\n";
	<< std fib(4);
	<< std "\n";
	<< std fib(5);

}