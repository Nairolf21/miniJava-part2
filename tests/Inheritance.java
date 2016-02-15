/* Expected memory
 *
 * Expected method table:
 *
 *  - Animal_walk
 *  - Cat_walk
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
 *  - Cat_walk
 *  - Cat_meow
 *  - Cat_die
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

    public void meow() {}

    public void die() {
        livesLeft--;
    }

}

/* Expected class descriptor:
 *
 * Expected method table names:
 *  - Animal_walk
 *
 * Exprected attribute list:
 *  - legNumber
 *  - name
 *
 * */
public class Animal {
    private int legNumber;
    private String name;

    public void walk() {}

}
