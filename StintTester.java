import java.util.ArrayList;

public class StintTester {

	static ArrayList<Stint> cases=new ArrayList<Stint>();

	private static void init(){
		log("INIT BEGIN",true);
		try{
			cases.add(new Stint());
			cases.add(new Stint("23"));
			cases.add(new Stint("abc"));
			cases.add(new Stint(true));
			cases.add(new Stint(false));
			cases.add(new Stint(123));
			cases.add(new Stint(""));
			Stint s2=new Stint("stint of stint");
			cases.add(new Stint(s2));
			cases.add(new Stint("da3232dsds2ds"));
			cases.add(new Stint("321dasd231"));
			cases.add(new Stint("23dasd231da"));
			cases.add(new Stint("das231dasd213"));
			log("..",false);
		}catch(Exception e){
			log("INIT FAILED",true);
			e.printStackTrace();
			return;
		}
		log("INIT PASSED",true);
	}

	public static void content(){
		log("INIT-CONTENT BEIGN",true);
		try{
			for(Stint s: cases){
				log(s.toString()+": ",false);
				log(s.integers.toString(),false);
				log(s.strings.toString(),false);
			}
		}catch(Exception e){
			log("INIT-CONTENT FAILED",true);
			e.printStackTrace();
			return;
		}
		log("INIT-CONTENT PASSED",true);
	}

	public static void checkEq(){
		log("EQUAL CHECK BEIGN",true);
		try{
			for(Stint s: cases){
				log(">>> "+s.toString()+" >>>",false);
				log("Check true:"+s.equals(s),false);
				log("Check false:"+s.nonEquals(s),false);
				log("Check false:"+s.equals(new Stint("$")),false);
				log("Check true:"+s.equals(new Stint(s.toString())),false);
				log("Check false:"+s.nonEquals(new Stint(s.toString())),false);
			}
		}catch(Exception e){
			log("EQUAL CHECK FAILED",true);
			e.printStackTrace();
			return;
		}
		log("EQUAL CHECK PASSED",true);
	}

	public static void checkAdd(){
		log("ADD CHECK BEIGN",true);
		try{
			for(Stint s: cases){
				log(">>> "+s.toString()+" >>>",false);
				Stint ad=s.add(new Stint("ab"));
				log(ad.toString(),false);
			}
		}catch(Exception e){
			log("ADD CHECK FAILED",true);
			e.printStackTrace();
			return;
		}
		log("ADD CHECK PASSED",true);
	}

	public static void checkAddAt(){
		log("@ADD CHECK BEIGN",true);
		try{
			Stint s1=new Stint("bbhj78gjh");
			for(int i=0;i<=s1.toString().length();i++){
				log(">>> "+s1.toString()+" >>>",false);
				Stint s2=s1.addAt(new Stint("@@@"), i);
				log(s2.toString(),false);
			}
		}catch(Exception e){
			log("@ADD CHECK FAILED",true);
			e.printStackTrace();
			return;
		}
		log("@ADD CHECK PASSED",true);
	}

	public static void checkGet(){
		log("@SUBSTRING CHECK BEGIN",true);
		try{
			for(int i=0;i<cases.size();i++){
				log(">>> "+cases.get(i).toString()+" >>>",false);
				for(int j=0;j<cases.get(i).toString().length();j++){
					Stint s2=cases.get(i).getSubstring(j);
					log(s2.toString(),false);
					s2=cases.get(i).getSubstring(j,cases.get(i).toString().length()-j);
					log(s2.toString(),false);
					if(j<cases.get(i).integers.size()){
						int s1=cases.get(i).getInt(j);
						log(s1+"",false);
					}
					if(j<cases.get(i).strings.size()){
						s2=cases.get(i).getString(j);
						log(s2.toString(),false);
					}
					log("----",false);
				}
			}
		}catch(Exception e){
			log("@SUBSTRING CHECK FAILED",true);
			e.printStackTrace();
			return;
		}
		log("@SUBSTRING CHECK PASSED",true);
	}
	
	public static void checkRemove(){
		log("@REMOVE CHECK BEGIN",true);
		try{
			Stint s=cases.get(8).clone();
			log(">>> "+s.toString()+" >>>",false);
			s.removeInt(0);
			log(s.toString(),false);
			s=cases.get(8).clone();
			s.removeInt(1);
			log(s.toString(),false);
			s=cases.get(8).clone();
			log(s.toString(),false);
			s.removeRange(0,1);
			log(s.toString(),false);
			s=cases.get(8).clone();
			s.removeRange(2,1);
			log(s.toString(),false);
			s=cases.get(8).clone();
			s.removeString(1);
			log(s.toString(),false);
			s=cases.get(8).clone();
			s=cases.get(8).clone();
			s.removeChar(1);
			log(s.toString(),false);
			s=cases.get(8).clone();
			
		}catch(Exception e){
			log("@REMOVE CHECK FAILED",true);
			e.printStackTrace();
			return;
		}
		log("@REMOVE CHECK PASSED",true);
	}

	public static void checkSet(){
		log("@SET CHECK BEGIN",true);
		try{
			Stint s=cases.get(8).clone();
			log(">>> "+s.toString()+" >>>",false);
			s.setByString(new Stint("~"),0);
			log(s.toString(),false);
			s=cases.get(8).clone();
			s.setByString(new Stint("~"),1);
			log(s.toString(),false);
			s=cases.get(8).clone();
			s.setByString(new Stint("~"),2);
			log(s.toString(),false);
			s=cases.get(8).clone();
			s.setByString(new Stint("4444"),0);
			log(s.toString(),false);
			s=cases.get(8).clone();
			log("----",false);
			for(int i=0;i<cases.size();i++){
				log(">>> "+cases.get(i).toString()+" >>>",false);
				for(int j=0;j<cases.get(i).toString().length();j++){
					s=cases.get(i).clone();
					s.setByIndex(new Stint("~"),j);
					log(s.toString(),false);
				}
			}
			log("----",false);
			s=cases.get(8).clone();
			log(">>> "+s.toString()+" >>>",false);
			s.setByInt(9999,0);
			log(s.toString(),false);
			s=cases.get(8).clone();
			s.setByInt(9999,1);
			log(s.toString(),false);
			s=cases.get(8).clone();
			s.setByInt(9999,2);
			log(s.toString(),false);
			s=cases.get(8).clone();
			s.setByInt(0,0);
			log(s.toString(),false);
			s=cases.get(8).clone();
			log("----",false);
			for(int i=0;i<cases.size();i++){
				log(">>> "+cases.get(i).toString()+" >>>",false);
				for(int j=0;j<cases.get(i).toString().length()-1;j++){
					s=cases.get(i).clone();
					s.setByRange(new Stint("~"),j,2);
					log(s.toString(),false);
				}
			}
			
			log("******",false);
			s=cases.get(8);
			log(s.toString(),false);
			int j=s.getCount(new Stint("2"));
			log(j+"",false);
			s.setByString(new Stint("@@"),0);
			log(s.toString(),false);
			
			log("******",false);
			s=cases.get(8);
			log(s.toString(),false);
			j=s.split(new Stint("32"));
			log(j+"",false);
			s.setByString(new Stint("**"),1);
			log(s.toString(),false);
			
		}catch(Exception e){
			log("@SET CHECK FAILED",true);
			e.printStackTrace();
			return;
		}
		log("@SET CHECK PASSED",true);
	}

	public static void main(String[] args){
		init();
		log("",false);
		content();
		log("",false);
		checkEq();
		log("",false);
		checkAdd();
		log("",false);
		checkAddAt();
		log("",false);
		checkGet();
		log("",false);
		checkRemove();
		log("",false);
		checkSet();
	}

	public static void log(String m,boolean sw){
		if(sw)
			System.err.println("<------------------"+m+"-------------------->");
		else
			System.err.println(m);
	}


}
