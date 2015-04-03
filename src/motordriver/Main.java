package motordriver;


/**
 * Created by beenotung on 1/2/15.
 */
public class Main {
    public static void main(String[] args) {
        System.out.println("start");
        MotorDriver motorDriver = new MotorDriver();
        motorDriver.start();
        System.out.println("done");
    }
}
