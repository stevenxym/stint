import java.util.Scanner;
import java.io.*; 
import java.io.IOException; 

class Test{ 

public static void main (String args[]) 

{
    Stint filename = new Stint("data.txt");
  try { 
	Utility.getFile(filename);
	Scanner in;
	PrintWriter pwriter;
;
  if (Utility.close(filename)) 
	System.out.print("Close file successfully.");
	} 
	catch (Exception e) { 
 System.err.println (e); }
;
  return ;
}


 }