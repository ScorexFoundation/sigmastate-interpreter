package gf2t.test;

import java.util.*;
import gf2t.*;

public class GF2_128Test {

    private static class GF2t {
        private static byte[] multTable = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x00, 0x02, 0x04, 0x06, 0x08, 0x0A, 0x0C, 0x0E, 0x10, 0x12, 0x14, 0x16, 0x18, 0x1A, 0x1C, 0x1E, 0x00, 0x03, 0x06, 0x05, 0x0C, 0x0F, 0x0A, 0x09, 0x18, 0x1B, 0x1E, 0x1D, 0x14, 0x17, 0x12, 0x11, 0x00, 0x04, 0x08, 0x0C, 0x10, 0x14, 0x18, 0x1C, 0x20, 0x24, 0x28, 0x2C, 0x30, 0x34, 0x38, 0x3C, 0x00, 0x05, 0x0A, 0x0F, 0x14, 0x11, 0x1E, 0x1B, 0x28, 0x2D, 0x22, 0x27, 0x3C, 0x39, 0x36, 0x33, 0x00, 0x06, 0x0C, 0x0A, 0x18, 0x1E, 0x14, 0x12, 0x30, 0x36, 0x3C, 0x3A, 0x28, 0x2E, 0x24, 0x22, 0x00, 0x07, 0x0E, 0x09, 0x1C, 0x1B, 0x12, 0x15, 0x38, 0x3F, 0x36, 0x31, 0x24, 0x23, 0x2A, 0x2D, 0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38, 0x40, 0x48, 0x50, 0x58, 0x60, 0x68, 0x70, 0x78, 0x00, 0x09, 0x12, 0x1B, 0x24, 0x2D, 0x36, 0x3F, 0x48, 0x41, 0x5A, 0x53, 0x6C, 0x65, 0x7E, 0x77, 0x00, 0x0A, 0x14, 0x1E, 0x28, 0x22, 0x3C, 0x36, 0x50, 0x5A, 0x44, 0x4E, 0x78, 0x72, 0x6C, 0x66, 0x00, 0x0B, 0x16, 0x1D, 0x2C, 0x27, 0x3A, 0x31, 0x58, 0x53, 0x4E, 0x45, 0x74, 0x7F, 0x62, 0x69, 0x00, 0x0C, 0x18, 0x14, 0x30, 0x3C, 0x28, 0x24, 0x60, 0x6C, 0x78, 0x74, 0x50, 0x5C, 0x48, 0x44, 0x00, 0x0D, 0x1A, 0x17, 0x34, 0x39, 0x2E, 0x23, 0x68, 0x65, 0x72, 0x7F, 0x5C, 0x51, 0x46, 0x4B, 0x00, 0x0E, 0x1C, 0x12, 0x38, 0x36, 0x24, 0x2A, 0x70, 0x7E, 0x6C, 0x62, 0x48, 0x46, 0x54, 0x5A, 0x00, 0x0F, 0x1E, 0x11, 0x3C, 0x33, 0x22, 0x2D, 0x78, 0x77, 0x66, 0x69, 0x44, 0x4B, 0x5A, 0x55,
        };

        private long[] x;

        public boolean isOne() {
            if (x[0] != 1l) return false;
            for (int i = 1; i < x.length; i++) {
                if (x[i] != 0l) return false;
            }
            return true;
        }

        public boolean equals(long[] that) {
            int i;
            for (i = 0; i < Math.min(x.length, that.length); i++) {
                if (x[i] != that[i])
                    return false;
            }
            for (; i < x.length; i++) {
                if (x[i] != 0) {
                    return false;
                }
            }
            for (; i < that.length; i++) {
                if (that[i] != 0) {
                    return false;
                }
            }
            return true;
        }


        public static void mulBits(GF2t ret, long[] p, long[] q) {
            long[] a = p, b = q;
            long[] c = new long[a.length + b.length];


            for (int i = 0; i < a.length; i++) {
                for (int i1 = 0; i1 < 64; i1++) {
                    for (int j = 0; j < b.length; j++) {
                        for (int j1 = 0; j1 < 64; j1++) {
                            if ((a[i] & (1l << i1)) != 0 && (b[j] & (1l << j1)) != 0) {
                                int cPosition = i * 64 + i1 + j * 64 + j1;
                                c[cPosition / 64] ^= 1l << (cPosition % 64);
                            }
                        }
                    }
                }
            }
            ret.x = c;
        }

        private static void modReduce(GF2t poly, Modulus mod) {
            for (int i = poly.x.length * 64 - 1; i >= mod.degree; i--) {
                if ((poly.x[i >> 6] & (1l << (i & 63))) != 0) {
                    for (int j = 0; j < mod.offset.length; j++) {
                        int k = i - mod.offset[j];
                        poly.x[k >> 6] ^= (1l << (k & 63));
                    }
                }
            }
        }

        public String toString() {
            String ret = "";
            for (int i = x.length - 1; i >= 0; i--) {
                ret += x[i];
            }
            return ret;
        }

        public static class Modulus {
            // represented a an array of bit positions
            // where coefficient = 1, counting from degree down
            private final int[] offset;
            private final int degree;

            Modulus(int[] sparseModulus) {
                degree = sparseModulus[0];
                offset = new int[sparseModulus.length];
                offset[0] = 0;
                for (int i = 1; i < sparseModulus.length; i++) {
                    offset[i] = degree - sparseModulus[i];
                }
            }
        }
    }

    public static void constructorAndEqualityTest() {
        System.out.println("Testing constructors, isZero, isOne, equality");
        GF2_128 t = new GF2_128();
        long[] r = t.toLongArray();
        if (!t.isZero() || r.length != 2 || r[1] != 0l || r[0] != 0l) {
            System.out.println("Fail: empty constructor.");

        }

        t = new GF2_128(0);
        r = t.toLongArray();
        if (!t.isZero() || r.length != 2 || r[1] != 0l || r[0] != 0l) {
            System.out.println("Fail: constructor on 0 int");
        }

        t = new GF2_128(1);
        r = t.toLongArray();
        if (!t.isOne() || r.length != 2 || r[1] != 0l || r[1] != 0l) {
            System.out.println("Fail: constructor on 1 int");
        }

        t = new GF2_128(-1);
        r = t.toLongArray();
        if (r[0] != 0xFFFFFFFFL || r[1] != 0l) {
            System.out.println("Fail: constructor on 0xFFFFFFFF int " + t);
        }

        long[] s = new long[2];

        s[0] = 123345l;
        s[1] = 123567891234567l;

        t = new GF2_128(s);

        GF2_128 t1 = new GF2_128(t);

        r = t.toLongArray();
        if (r[0] != s[0] || r[1] != s[1]) {
            System.out.println("Fail: constructor on long array");
        }


        r = t1.toLongArray();
        if (r[0] != s[0] || r[1] != s[1]) {
            System.out.println("Fail: copy constructor");
        }

        byte[] b = new byte[16];
        for (int i = 0; i < 8; i++) {
            b[i] = (byte) (r[0] >>> (i * 8));
        }

        for (int i = 0; i < 8; i++) {
            b[i + 8] = (byte) (r[1] >>> (i * 8));
        }

        t = new GF2_128(b);
        s = t.toLongArray();
        if (r[0] != s[0] || r[1] != s[1]) {
            System.out.println("Fail: constructor on byte array");
        }

        s[0] = 0xFFFFFFFFFFFFFFFFL;
        s[1] = 0xFFFFFFFFFFFFFFFFL;

        t = new GF2_128(s);

        t1 = new GF2_128(t);

        r = t.toLongArray();
        if (r[0] != s[0] || r[1] != s[1]) {
            System.out.println("Fail: constructor on long array of all 1s");
        }


        r = t1.toLongArray();
        if (r[0] != s[0] || r[1] != s[1]) {
            System.out.println("Fail: copy constructor");
        }

        for (int i = 0; i < 8; i++) {
            b[i] = (byte) (r[0] >>> (i * 8));
        }

        for (int i = 0; i < 8; i++) {
            b[i + 8] = (byte) (r[1] >>> (i * 8));
        }

        t = new GF2_128(b);
        s = t.toLongArray();
        if (r[0] != s[0] || r[1] != s[1]) {
            System.out.println("Fail: constructor on byte array of all 1s");
        }

    }

    //TODO: write addition tests
    // TODO: write tests where arguments and/or results are same and/or different objects

    public static void mulAndInvertTest() {

        GF2t res1 = new GF2t();
        int[] pentanomial = {128, 7, 2, 1, 0};
        GF2t.Modulus m = new GF2t.Modulus(pentanomial);

        long[][] testValues = new long[1000][];
        GF2_128 res = new GF2_128();

        for (int i = 0; i < testValues.length; i++) {
            testValues[i] = new long[2];
        }


        // Test single 1s in every bit position but last
        // (1s will be tested separately)
        int j = 0;
        for (int i = 1; i < 64; i++) {
            testValues[j][0] = 1l << i;
            testValues[j++][1] = 0;
        }
        for (int i = 0; i < 64; i++) {
            testValues[j][0] = 0;
            testValues[j++][1] = 1l << i;
        }

        // Test single bytes
        for (int i = 0; i < 256; i++) {
            testValues[j][0] = i;
            testValues[j++][1] = 0;
        }

        // Test first half zero, second half random,
        // and first half random, second half 0
        // and first half random, second half 1

        Random rand = new Random();

        for (int i = 0; i < 100; i++) {
            testValues[j][0] = rand.nextLong();
            testValues[j++][1] = 0;
        }

        for (int i = 0; i < 100; i++) {
            testValues[j][0] = 0;
            testValues[j++][1] = rand.nextLong();
        }

        for (int i = 0; i < 100; i++) {
            testValues[j][0] = rand.nextLong();
            testValues[j++][1] = 1;
        }

        // Test both halves random
        while (j < testValues.length) {
            testValues[j][0] = rand.nextLong();
            testValues[j++][1] = rand.nextLong();
        }


        // Run everything times 0 and 0 times everything
        // and everything times 1 and 1 times everything
        // where 0 and 1 are GF2_128
        long[] temp = new long[2];
        GF2_128 zero = new GF2_128(temp);
        temp[0] = 1l;
        GF2_128 one = new GF2_128(temp);


        System.out.println("Testing squaring");

        // Test squaring
        GF2_128.sqr(res, zero);

        if (!res.isZero()) {
            System.out.println("Fail: square16 of 0");
            return;
        }
        GF2_128.sqr(res, one);
        if (!res.isOne()) {
            System.out.println("Fail: square16 of 1");
            return;
        }

        for (long[] p : testValues) {
            GF2_128.sqr(res, new GF2_128(p));
            GF2t.mulBits(res1, p, p);
            GF2t.modReduce(res1, m);
            if (!res1.equals(res.toLongArray())) {
                System.out.println("Fail: square16 " + new GF2_128(p));
                return;
            }
        }


        System.out.println("Testing power 65535");

        // Test power 65536
        GF2_128.pow65536(res, zero);
        if (!res.isZero()) {
            System.out.println("Fail: pow65536 of 0");
            return;
        }
        GF2_128.pow65536(res, one);
        if (!res.isOne()) {
            System.out.println("Fail: pow65536 of 1");
            return;
        }

        GF2_128 res2 = new GF2_128();
        for (long[] p : testValues) {
            GF2_128 p1 = new GF2_128(p);
            GF2_128.pow65536(res, p1);
            GF2_128.sqr(res2, p1);
            for (int k = 1; k < 16; k++) {
                GF2_128.sqr(res2, res2);
            }

            if (!res.equals(res2)) {
                System.out.println("Fail: pow65536 " + new GF2_128(p));
                return;
            }
        }

        // Run everything times 0 and 0 times everything
        // and everything times 1 and 1 times everything
        // where 0 and 1 are GF2_128


        System.out.println("Testing multiplication by 0 and 1");

        for (long[] p : testValues) {
            GF2_128 p1 = new GF2_128(p);
            GF2_128.mul(res, p1, zero);
            if (!res.isZero()) {
                System.out.println("Fail: " + p1 + " * 0");
                return;
            }
            GF2_128.mul(res, zero, p1);
            if (!res.isZero()) {
                System.out.println("Fail: 0 * " + p1);
            }
            GF2_128.mul(res, p1, one);
            if (!res.equals(p1)) {
                System.out.println("Fail: " + p1 + " * 1");
                return;
            }
            GF2_128.mul(res, one, p1);
            if (!res.equals(p1)) {
                System.out.println("Fail: 1 * " + p1);
                return;
            }
        }

        // Run everything times 0
        // and everything times 1
        // where 0 and 1 are bytes
        for (long[] p : testValues) {
            GF2_128 p1 = new GF2_128(p);
            GF2_128.mul(res, p1, (byte) 0);
            if (!res.isZero()) {
                System.out.println("Fail: " + p1 + " * 0 byte");
                return;
            }
            GF2_128.mul(res, p1, (byte) 1);
            if (!res.equals(p1)) {
                System.out.println("Fail: " + p1 + " * 1 byte ");
                return;
            }
        }

        System.out.println("Testing multiplication by single bytes");

        // Run everything times every byte
        temp = new long[1];
        for (long[] p : testValues) {
            GF2_128 p1 = new GF2_128(p);
            for (int i = 2; i < 256; i++) {
                temp[0] = i;
                GF2_128.mul(res, p1, (byte) i);
                GF2t.mulBits(res1, p, temp);
                GF2t.modReduce(res1, m);
                if (!res1.equals(res.toLongArray())) {
                    System.out.println("Fail: " + p1 + " * " + i + " byte");
                    return;
                }
            }
        }

        System.out.println("Testing general multiplication");

        // Now run everything times everything in the test array
        // TODO: speed this up
        for (long[] p : testValues) {
            GF2_128 p1 = new GF2_128(p);
            for (long[] q : testValues) {
                GF2_128 q1 = new GF2_128(q);
                GF2_128.mul(res, p1, new GF2_128(q1));
                GF2t.mulBits(res1, p, q);
                GF2t.modReduce(res1, m);
                if (!res1.equals(res.toLongArray())) {
                    System.out.println("Fail: " + p1 + " * " + q1);
                    return;
                }
            }
        }

        System.out.println("Testing general inversion");

        // Test inversion of 1
        GF2_128.invert(res, one);
        if (!res.isOne()) {
            System.out.println("Fail: inversion of 1");
            //return;

        }

        // Test inversion of everything
        for (long[] p : testValues) {
            GF2_128 p1 = new GF2_128(p);
            if (p1.isZero()) continue;
            GF2_128.invert(res, p1);
            GF2_128.mul(res2, p1, res);
            if (!res2.isOne()) {
                System.out.println("Fail: inversion of " + p1 + " self-test ");
            }
            GF2t.mulBits(res1, res.toLongArray(), p);
            GF2t.modReduce(res1, m);
            if (!res1.isOne()) {
                System.out.println("Fail: inversion of " + p1 + " gf2t-test");
            }
        }

    }

    public static void interpolateTest() {
        System.out.println("Testing interpolation and evaluation");


        // TODO: test behavior on null arguments

        Random rand = new Random();

        for (int len = 1; len < 100; len++) {
            byte[] points = new byte[len];
            GF2_128[] values = new GF2_128[len];
            byte[] temp = new byte[16];
            for (int i = 0; i < len; i++) {
                // generate a byte that is not equal to anything in the array nor 0
                while (true) {
                    byte b;
                    do {
                         b = (byte) rand.nextInt();
                    } while (b==(byte)0);
                    int j;
                    for (j = 0; j < i; j++) {
                        if (b == points[j]) { // detected equality with something in the array
                            break;
                        }
                    }
                    if (j == i) { // did not detect equality with anything in the array
                        points[i] = b;
                        break;
                    }
                }
            }
            for (int i = 0; i < len; i++) {
                rand.nextBytes(temp);
                values[i] = new GF2_128(temp);
            }

            GF2_128_Poly res = GF2_128_Poly.interpolate(points, values, Optional.empty());
            for (int i = 0; i < len; i++) {
                GF2_128 t = res.evaluate(points[i]);
                if (!t.equals(values[i])) {
                    System.out.println("Interpolation error on length = " + len + " at input point number " + i);
                }
            }
            rand.nextBytes(temp);
            GF2_128 valueAt0 = new GF2_128(temp);
            res = GF2_128_Poly.interpolate(points, values, Optional.of(valueAt0));
            for (int i = 0; i < len; i++) {
                GF2_128 t = res.evaluate(points[i]);
                if (!t.equals(values[i])) {
                    System.out.println("Interpolation error on length =  " + len + " at input point number " + i + "(with optional 0)");
                }

            }
            GF2_128 t = res.evaluate((byte) 0);
            if (!t.equals(valueAt0)) {
                System.out.println("Interpolation error on length =  " + len + " at input optional 0");
                System.out.println(Arrays.toString(points));
                System.out.println(Arrays.toString(values));
                System.out.println(valueAt0);
                System.out.println(res);
                System.out.println(t);
            }
        }
        for (int len = 1; len < 100; len++) {
            byte[] points = new byte[len];
            GF2_128[] values = new GF2_128[len];
            byte[] temp = new byte[16];

            for (int i = 0; i < len; i++) {
                // generate a byte that is not equal to anything in the array (but may be 0)
                while (true) {
                    byte b = (byte) rand.nextInt();
                    int j;
                    for (j = 0; j < i; j++) {
                        if (b == points[j]) { // detected equality with something in the array
                            break;
                        }
                    }
                    if (j == i) { // did not detect equality with anything in the array
                        points[i] = b;
                        break;
                    }
                }
            }
            for (int i = 0; i < len; i++) {
                rand.nextBytes(temp);
                values[i] = new GF2_128(temp);
            }

            GF2_128_Poly res = GF2_128_Poly.interpolate(points, values, Optional.empty());
            for (int i = 0; i < len; i++) {
                GF2_128 t = res.evaluate(points[i]);
                if (!t.equals(values[i])) {
                    System.out.println("Interpolation error on length =  " + len + " " + i + "(with 0 allowed but not additional)");
                }
            }
        }
    }


    public static void testAll() {
        constructorAndEqualityTest();
        mulAndInvertTest();
        interpolateTest();
    }

    public static void time() {
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

/*        t1 = System.nanoTime();
        for (int i = 0; i<1000000000; i++) {
            GF2_128.sqr(res, p);
        }
        t2 = System.nanoTime();
        System.out.println((t2-t1)/1000000.0);

        long t3 = System.nanoTime();
        for (int i = 0; i<100000000; i++) {
            GF2_128.mul(res, p,p);
        }
        long t4 = System.nanoTime();
        System.out.println((t4-t3)/1000000.0);

        System.out.println(((double)t4-t3)/(t2-t1)/10.0);
*/
   /*     t1 = System.nanoTime();
        for (int i = 0; i<50000000; i++) {
            GF2_128.pow65536(res, p);
        }
        t2 = System.nanoTime();
        System.out.println((t2-t1)/1000000.0);

        */
    }

}
