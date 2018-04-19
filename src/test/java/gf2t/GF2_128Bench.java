package gf2t;

import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.Random;

@RunWith(ReadableTest.class)
public class GF2_128Bench {

    @Test
    public void benchmark() {
        GF2_128 p;
        long[] d = new long[2];
        GF2_128 res = new GF2_128();
        Random rand = new Random();
        d[0] = rand.nextLong();
        d[1] = rand.nextLong();
        p = new GF2_128(d);
        long t1, t2, t3, t4;



        t1 = System.nanoTime();
        for (int i = 0; i < 10000000; i++) {
            d[0] = rand.nextLong();
            d[1] = rand.nextLong();
            p = new GF2_128(d);
            GF2_128.invert(res, p);
        }

        t2 = System.nanoTime();

        System.out.println((t2 - t1) / 10000000.0);

        t1 = System.nanoTime();
        for (int i = 0; i<1000000000; i++) {
            GF2_128.sqr(res, p);
        }
        t2 = System.nanoTime();
        System.out.println((t2-t1)/1000000.0);

        t3 = System.nanoTime();
        for (int i = 0; i<100000000; i++) {
            GF2_128.mul(res, p,p);
        }
        t4 = System.nanoTime();
        System.out.println((t4-t3)/1000000.0);

        System.out.println(((double)t4-t3)/(t2-t1)/10.0);
        t1 = System.nanoTime();
        for (int i = 0; i<50000000; i++) {
            GF2_128.pow65536(res, p);
        }
        t2 = System.nanoTime();
        System.out.println((t2-t1)/1000000.0);
    }
}
