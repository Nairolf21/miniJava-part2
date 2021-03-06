/* Expected memory
 *
 * Expected method table:
 *
 *  - Object_toString
 *  - Animal_walk
 *  - Animal_run
 *  - Cat_run
 *  - Cat_meow
 *  - Cat_die
 *
 * Expected class descriptor list:
 * 
 *  - Animal
 *  - Cat
 *
 * */


/* Expected class descriptor: 
 *
 * Expected method table names:
 *  - toString -> Object_toString
 *  - walk     -> Animal_walk
 *  - run      -> Cat_run
 *  - meow     -> Cat_meow
 *  - die      -> Cat_die
 *
 * Exprected attribute list:
 *  - legNumber
 *  - name
 *  - livesLeft
 *
 * */
public class Cat extends Animal {

    /* livesLeft should be between 0 and 9 */
    private int livesLeft;

	public void run() {
		String cat_run;
	}

    public void meow() {
		String cat_meow;
	}

    public void die() {
        livesLeft--;
    }


}

/* Expected class descriptor:
 *
 * Expected method table names:
 *  - toString -> Object_toString
 *  - walk     -> Animal_walk
 *  - run      -> Animal_run
 *
 * Exprected attribute list:
 *  - legNumber
 *  - name
 *
 * */
public class Animal {
    private int legNumber;
    private String name;

    public void walk() {
		String animal_walk;
	}
	public void run() {
		String animal_run;
	}

}

public class Main {

    public static void main(String[] args) {
        2;
        2.5;
        2 + 3;
        true;
        new Animal();
        //Animal newAnimal = new Animal();
        int a;
        float b;
        Animal animal_test;
        int[] array_test;
        int c, d;
        boolean uninitialized_bool;

        //Variable declaration with init value
        int initialized_int = 2;
        float initialized_float = 2.5;
        booleandkfg initialized_bool = true;

        //Initialization with arithmetic expressions, to test evaluation
        //of arithmetic expressions
        int add_int = 2 + 3;
        float add_float = 2.2 + 3.5;
        float add_float2 = 2 + 3.5;
        float add_float3 = 2.5 + 3;
        int mult_int = 2 * 3;
        int mult_add_int = 2 * 3 + 5;
        float mult_float = 2.5 * 3;
        float mult_add_float = 2 * 3.5 + 5;
        int div_int = 6 / 2;
        int div_int2 = 5 / 2;
        int mod_int = 5 % 2;

        boolean cor_boolean = true || false;
        boolean cor_boolean2 = false || true;
        boolean cor_boolean3 = true || true;
        boolean cor_boolean4 = false || false;

        boolean gt1 = 2 > 2;
        boolean gt2 = 2. > 2;
        boolean gt3 = 2 > 2.;
        boolean gt4 = 3 > 2;
        boolean gt5 = 1 > 2;
        boolean gt6 = 3. > 2.;

        boolean lt1 = 3 < 2;
        boolean lt2 = 2 < 2;
        boolean lt3 = 1 < 2;

        boolean le1 = 3 <= 2;
        boolean le2 = 2 <= 2;
        boolean le3 = 1 <= 2;

        boolean ge1 = 3 >= 2;
        boolean ge2 = 2 >= 2;
        boolean ge3 = 1 >= 2;

        boolean eq1 = 3 == 2;
        boolean eq2 = 2 == 2;
        boolean eq3 = 1 == 2;

        boolean ne1 = 3 != 2;
        boolean ne2 = 2 != 2;
        boolean ne3 = 1 != 2;

        //Trying to find value in stack by name
        //add_int;

    }
}
