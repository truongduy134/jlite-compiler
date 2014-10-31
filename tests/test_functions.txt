class Main {

void main(){
	Int a; 
	Int b;
	Int i;
	Int d;
	Int t1;
	Int t2;
	Compute help;

	a = 1;
	b = 2;
	i = 3;
	d = 4;

	help = new Compute();

	t1 = help.addSquares(a,b) + help.square(i); // Should be equal to 14 (1 + 4 + 9)
	t2 = help.square(d); // Should be equal to 16

	if(t2>t1){
		println("Square of d larger than sum of squares"); 
		// Should be the output
	}
	else{
		println("Square of d smaller than sum of squares");
	}
}
}

class Compute {

   Bool computedSquares;
   Int chachedValue;

   Int square(Int a){
     return a*a;
   }
   
   Int add(Int a, Int b){
    return a+b;
   }
   
   Int addSquares(Int a, Int b){
    if(computedSquares){
      return chachedValue;
    }
    else{
      computedSquares = true;
      return add(square(a),square(b));
    }
   }
}