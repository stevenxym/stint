import java.util.Scanner;
import java.io.*; 
import java.io.IOException; 

class BuildInFunctions{ 

static Stint readFile(Stint filename)

{
    Stint s = new Stint("");
  	while (Utility.getScanner(filename).hasNextLine()) {
  s = new Stint(Utility.getScanner(filename).nextLine());
 System.out.println(s.toString());
 };
  return s;
}


static boolean printFile(Stint s, Stint filename)

{
  	try { 
	Utility.getFile(filename);
;
  	Utility.getWriter(filename).print((s).toString());
  	if (Utility.close(filename)) 
	System.out.print("Close file successfully.");
	} 
	catch (Exception e) { 
 System.err.println (e); }
;
  return true;
}


static Stint replaceAll(Stint dest, Stint d, Stint s)

{
    int n = dest.getCount(d);
  while (n > 0) 
  {
    dest.setByString(s, 0);
    n = dest.getCount(d);
  }

  return dest;
}


static Stint toUpperCase(Stint s)

{
  replaceAll(s, new Stint("a"), new Stint("A"));
  replaceAll(s, new Stint("b"), new Stint("B"));
  replaceAll(s, new Stint("c"), new Stint("C"));
  replaceAll(s, new Stint("d"), new Stint("D"));
  replaceAll(s, new Stint("e"), new Stint("E"));
  replaceAll(s, new Stint("f"), new Stint("F"));
  replaceAll(s, new Stint("g"), new Stint("G"));
  replaceAll(s, new Stint("h"), new Stint("H"));
  replaceAll(s, new Stint("i"), new Stint("I"));
  replaceAll(s, new Stint("j"), new Stint("J"));
  replaceAll(s, new Stint("k"), new Stint("K"));
  replaceAll(s, new Stint("l"), new Stint("L"));
  replaceAll(s, new Stint("m"), new Stint("M"));
  replaceAll(s, new Stint("n"), new Stint("N"));
  replaceAll(s, new Stint("o"), new Stint("O"));
  replaceAll(s, new Stint("p"), new Stint("P"));
  replaceAll(s, new Stint("q"), new Stint("Q"));
  replaceAll(s, new Stint("r"), new Stint("R"));
  replaceAll(s, new Stint("s"), new Stint("S"));
  replaceAll(s, new Stint("t"), new Stint("T"));
  replaceAll(s, new Stint("u"), new Stint("U"));
  replaceAll(s, new Stint("v"), new Stint("V"));
  replaceAll(s, new Stint("w"), new Stint("W"));
  replaceAll(s, new Stint("x"), new Stint("X"));
  replaceAll(s, new Stint("y"), new Stint("Y"));
  replaceAll(s, new Stint("z"), new Stint("Z"));
  return s;
}


static Stint toLowerCase(Stint s)

{
  replaceAll(s, new Stint("A"), new Stint("a"));
  replaceAll(s, new Stint("B"), new Stint("b"));
  replaceAll(s, new Stint("C"), new Stint("c"));
  replaceAll(s, new Stint("D"), new Stint("d"));
  replaceAll(s, new Stint("E"), new Stint("e"));
  replaceAll(s, new Stint("F"), new Stint("f"));
  replaceAll(s, new Stint("G"), new Stint("g"));
  replaceAll(s, new Stint("H"), new Stint("h"));
  replaceAll(s, new Stint("I"), new Stint("i"));
  replaceAll(s, new Stint("J"), new Stint("j"));
  replaceAll(s, new Stint("K"), new Stint("k"));
  replaceAll(s, new Stint("L"), new Stint("l"));
  replaceAll(s, new Stint("M"), new Stint("m"));
  replaceAll(s, new Stint("N"), new Stint("n"));
  replaceAll(s, new Stint("O"), new Stint("o"));
  replaceAll(s, new Stint("P"), new Stint("p"));
  replaceAll(s, new Stint("Q"), new Stint("q"));
  replaceAll(s, new Stint("R"), new Stint("r"));
  replaceAll(s, new Stint("S"), new Stint("s"));
  replaceAll(s, new Stint("T"), new Stint("t"));
  replaceAll(s, new Stint("U"), new Stint("u"));
  replaceAll(s, new Stint("V"), new Stint("v"));
  replaceAll(s, new Stint("W"), new Stint("q"));
  replaceAll(s, new Stint("X"), new Stint("x"));
  replaceAll(s, new Stint("Y"), new Stint("y"));
  replaceAll(s, new Stint("Z"), new Stint("z"));
  return s;
}

public static void main(String[] args){
	Stint s=new Stint("ABC");
	Stint s1=new Stint("abc");
	System.out.println(toLowerCase(s).toString());
	System.out.println(toUpperCase(s1).toString());
}

 }
