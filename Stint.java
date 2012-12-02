import java.util.ArrayList;

public class Stint{
	
	StringBuilder content;
	ArrayList<Integer> integers;
	ArrayList<String> strings;
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
		return integers.get(0);
	}

	public Stint getString(int index){
		if(spliter==null && chooser==null){
			if(integers.size()==0){
				exception("Stint: Invalid Index");
			}
			return new Stint(strings.get(0));
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
		return this.toString().split(spliter).length;
	}

	public int getCount(Stint s){
		return this.toString().split(chooser).length-1;
	}

	public Stint removeInt(int index){
		return null;
	}

	public Stint remove(int start, int end){
		return null;
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
	
	private void exception(String message){
		throw new RuntimeException(message);
	}

}
