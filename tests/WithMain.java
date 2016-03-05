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
        true;

    
    }
}
