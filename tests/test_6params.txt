class Main {

	void main(){
		Int a; 
		Compute help;
		Bool b;
		
		b = false;
		b = ((!b) || false) && (b);
		
		if(help.add() == NULL ){
			println("NULL return"); 
		}
		else{
			println("Not NULL return"); 
		}
		
		help = new Compute();
		a = help.add(1,2,3,4,5);
		
		println("\nSum of 5 numbers:"); 
		println(a);
		
		a = help.add(-5,6,7,8,9);
		println("\nSum of other 5 numbers:"); 
		println(a);
		
		
	}
}

class Compute {

	Int x;
	Int y;
	Int z;

   Int add(Int a, Int b, Int c, Int d, Int e){
     return (a + b + c + d - -e);
   }   
   
   Compute add(){
	return NULL;
   }
}