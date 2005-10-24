public class getsp {
    public static void main(String[] args) {
	if (args!=null && args.length>0) {
	    if (args[0].compareTo("-test")==0) {
		System.out.println("Test1234OK");
	    } else
	    if (args[0].compareTo("-libs")==0) {
		String prefix="-L";
		if (args.length>1) prefix=args[1];
		String lp=System.getProperty("java.library.path");
		// we're not using StringTokenizer in case the JVM is very crude
		int i=0,j,k=0;
		String r=null;
		String pss=System.getProperty("path.separator");
		char ps=':';
		if (pss!=null && pss.length()>0) ps=pss.charAt(0);
		j=lp.length();
		while (i<=j) {
		    if (i==j || lp.charAt(i)==ps) {
			String lib=lp.substring(k,i);
			k=i+1;
			if (lib.compareTo(".")!=0)
			    r=(r==null)?(prefix+lib):(r+" "+prefix+lib);
		    }
		    i++;
		}
		if (r!=null) System.out.println(r);
	    } else
		System.out.println(System.getProperty(args[0]));
	}
    }
}
