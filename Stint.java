import java.util.ArrayList;
import java.util.TreeMap;

public class Stint{
	
	StringBuilder content;
	TreeMap<Integer,Integer> integers;
	TreeMap<Integer, String> strings;
	String spliter;
	String chooser;

	public Stint(){
		this("");
	}

	public Stint(String arg){
		content=new StringBuilder();
		content.append(arg);
		update();
	}

	public Stint(int arg){
		this(arg+"");
	}

	public Stint(boolean arg){
		this(arg?"true":"false");
	}

	/* NEVER call this directly */
	public String toString(){
		return content.toString();
	}

	public boolean equals(Stint s){
		return content.toString().equals(s.toString());
	}
	
	public boolean nonEquals(Stint s){
		return !equals(s);
	}

	public Stint add(Stint s){
		String s1=this.toString();
		String s2=s.toString();
		return new Stint(s1+s2);
	}

	public Stint addAt(Stint s, int index){
		String s1=this.toString();
		String s2=s.toString();
		if(s2.length()-1<index || index<0){
			exception("Stint: Invalid Index");
			return this;
		}
		return new Stint(s1.substring(0,index+1)+s2+s1.substring(index+1,s1.length()));
	}

	public Stint minus(Stint s){
		String s1=this.toString();
		String s2=s.toString();
		if(s1.indexOf(s2)==-1)
			return this;
		else{
			return new Stint(s1.replaceAll(s2, ""));
		}
	}
	
	public Stint minusAt(Stint s, int index){
		String s1=this.toString().substring(index);
		String s2=s.toString();
		if(s1.indexOf(s2)==-1)
			return this;
		else{
			return new Stint(s1.replaceAll(s2, ""));
		}
	}

	public Stint getSubstring(int index){
		if(index>=this.toString().length()){
			exception("Stint: Invalid Index");
			return this;
		}
		return new Stint(this.toString().substring(index, index+1));
	}

	public Stint getSubstring(int start, int length){
		if(start+length>this.toString().length()){
			exception("Stint: Invalid Length");
			return this;
		}
		return new Stint(this.toString().substring(start, start+length));
	}

	public int getInt(int index){
		if(integers.size()==0){
			exception("Stint: Invalid Index");
		}
		int t=0;
		int key=0;
		for(Integer i:integers.keySet()){
			if(t==index)
				key=i;
			else t++;
		}
		return integers.get(key);
	}

	public Stint getString(int index){
		if(spliter==null && chooser==null){
			if(strings.size()==0){
				exception("Stint: Invalid Index");
			}
			int t=0;
			int key=0;
			for(Integer i:strings.keySet()){
				if(t==index)
					key=i;
				else t++;
			}
			return new Stint(strings.get(key));
		}else if(spliter!=null){
			String[] temp=this.toString().split(spliter);
			if(temp.length-1<index)
				exception("Stint: Invalid Index");
			return new Stint(temp[index]);
		}else{
			return new Stint(chooser);
		}
	}

	public int split(Stint s){
		spliter=s.toString();
		chooser=null;
		return this.toString().split(spliter).length;
	}

	public int getCount(Stint s){
		chooser=s.toString();
		spliter=null;
		return this.toString().split(chooser).length-1;
	}

	public Stint removeInt(int index){
		if(index>integers.size()-1)
			exception("Stint: Invalid Index");
		else{
			int t=0;
			int key=0;
			for(Integer i:integers.keySet()){
				if(t==index)
					key=i;
				else t++;
			}
			integers.remove(key);
			String temp=reBuild();
			content=new StringBuilder();
			content.append(key);
			update();
		}
		return this;
	}

	public Stint remove(int start, int length){
		String temp=content.toString();
		if(start==0)
			temp=temp.substring(length);
		else temp=temp.substring(0,start)+temp.substring(start+length);
		content=new StringBuilder();
		content.append(temp);
		update();
		return this;
	}
	
	public void setByString(Stint s,int index){
		
	}
	
	public void setByIndex(Stint s, int index){
		
	}
	
	public void setByInt(int value, int index){
		
	}
	
	public void setByRange(int start, int length){
		
	}

	/* Below are private methods for the maintaince of internal structure */

	private void update(){

	}
	
	private String reBuild(){
		return null;
	}
	
	private void exception(String message){
		throw new RuntimeException(message);
	}

}
