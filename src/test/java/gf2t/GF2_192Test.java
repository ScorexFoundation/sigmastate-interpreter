package gf2t;

import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.Random;
import java.util.Arrays;

import static org.junit.Assert.assertFalse;

@RunWith(ReadableTest.class)
public class GF2_192Test {

    private static class GF2t_slow {

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


        public static void mulBits(GF2t_slow ret, long[] a, long[] b) {
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

        private static void modReduce(GF2t_slow poly, Modulus mod) {
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
            // represented as an array of bit positions
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

    private static long[][] testValues = null;
    private static GF2_192 zero = new GF2_192(0);
    private static GF2_192 one = new GF2_192(1);
    private static int[] pentanomial = {192, 7, 2, 1, 0};
    private static GF2t_slow.Modulus m = new GF2t_slow.Modulus(pentanomial);
    static {genTestValues();}
    private static void genTestValues() {
        if (testValues == null) {
            testValues = new long[250][];

            for (int i = 0; i < testValues.length; i++) {
                testValues[i] = new long[3];
            }


            // Test single 1s in every bit position but last
            // (1s in last bit position -- i.e., just the value of 1 -- will be tested separately)
            int j = 0;
            for (int i = 1; i < 64; i++, j++) {
                testValues[j][0] = 1L << i;
                testValues[j][1] = 0;
                testValues[j][2] = 0;
            }
            for (int i = 0; i < 64; i++, j++) {
                testValues[j][0] = 0;
                testValues[j][1] = 1L << i;
                testValues[j][2] = 0;
            }
            for (int i = 0; i < 64; i++, j++) {
                testValues[j][0] = 0;
                testValues[j][1] = 0;
                testValues[j][2] = 1L << i;
            }

            // Test first word zero, last two words random,
            // and first word random, last two words 0
            // and first word random, second word 1, last word 0
            // and last word random, first two words 0

            Random rand = new Random();

            for (int i = 0; i < 5; i++, j++) {
                testValues[j][0] = 0;
                testValues[j][1] = rand.nextLong();
                testValues[j][2] = rand.nextLong();
            }

            for (int i = 0; i < 5; i++, j++) {
                testValues[j][0] = rand.nextLong();
                testValues[j][1] = 0;
                testValues[j][2] = 0;
            }


            for (int i = 0; i < 5; i++, j++) {
                testValues[j][0] = rand.nextLong();
                testValues[j][1] = 1;
                testValues[j][2] = 0;
            }

            for (int i = 0; i < 5; i++, j++) {
                testValues[j][0] = 0;
                testValues[j][1] = 1;
                testValues[j][2] = rand.nextLong();
            }

            // Test all three words random
            while (j < testValues.length) {
                testValues[j][0] = rand.nextLong();
                testValues[j][1] = rand.nextLong();
                testValues[j++][2] = rand.nextLong();
            }
        }
    }

    @Test
    public void constructorAndEqualityTest() {
        GF2_192 t = new GF2_192();
        long[] r = t.toLongArray();
        assertFalse("Fail: empty constructor.", !t.isZero() || r.length != 3 || r[0] != 0L || r[1] != 0L || r[2] != 0L);

        t = new GF2_192(0);
        r = t.toLongArray();
        assertFalse("Fail: constructor on 0 int",!t.isZero() || r.length != 3 || r[0] != 0L || r[1] != 0L || r[2] != 0L);

        t = new GF2_192(1);
        r = t.toLongArray();
        assertFalse("Fail: constructor on 1 int", !t.isOne() || r.length != 3 || r[0] != 1L || r[1] != 0L || r[2] != 0L);

        t = new GF2_192(-1);
        r = t.toLongArray();
        assertFalse("Fail: constructor on 0xFFFFFFFF int " + t, r[0] != 0xFFFFFFFFL || r[1] != 0L || r[2]!=0L);

        long[] s = new long[3];

        s[0] = 123345L;
        s[1] = 123567891234567L;
        s[2] = 487237823242367L;

        t = new GF2_192(s);

        GF2_192 t1 = new GF2_192(t);

        r = t.toLongArray();
        assertFalse("Fail: constructor on long array", r[0] != s[0] || r[1] != s[1] || r[2] != s[2]);


        r = t1.toLongArray();
        assertFalse ("Fail: copy constructor",r[0] != s[0] || r[1] != s[1] || r[2] != s[2]) ;

        byte[] b = new byte[24];
        for (int i = 0; i < 8; i++) {
            b[i] = (byte) (r[0] >>> (i * 8));
        }

        for (int i = 0; i < 8; i++) {
            b[i + 8] = (byte) (r[1] >>> (i * 8));
        }

        for (int i = 0; i < 8; i++) {
            b[i + 16] = (byte) (r[2] >>> (i * 8));
        }
        t = new GF2_192(b);
        s = t.toLongArray();

        assertFalse("Fail: constructor on byte array",r[0] != s[0] || r[1] != s[1] || r[2] != s[2]);
        byte [] c = t.toByteArray();
        assertFalse("Fail: toByteArray", !Arrays.equals(b, c));

        byte [] b2 = new byte[30];
        for (int i=0; i<24; i++) {
            b2[i+6]=b[i];
        }
        t = new GF2_192(b2, 6);
        s = t.toLongArray();
        assertFalse("Fail: constructor on byte array with offset", r[0] != s[0] || r[1] != s[1] || r[2]!=s[2]);

        byte [] b1 = t.toByteArray();
        assertFalse("Fail: toByteArray", !Arrays.equals(b, b1));


        byte [] b3 = new byte [40];
        t.toByteArray(b3, 10);
        for (int i = 0; i<b.length; i++) {
            assertFalse("Fail: toByteArray with offset", b3[i+10]!=b[i]);
        }


        s[0] = 0xFFFFFFFFFFFFFFFFL;
        s[1] = 0xFFFFFFFFFFFFFFFFL;
        s[2] = 0xFFFFFFFFFFFFFFFFL;

        t = new GF2_192(s);

        t1 = new GF2_192(t);

        r = t.toLongArray();
        assertFalse("Fail: constructor on long array of all 1s", r[0] != s[0] || r[1] != s[1] || r[2] != s[2]);


        r = t1.toLongArray();
        assertFalse("Fail: copy constructor", r[0] != s[0] || r[1] != s[1] || r[2] != s[2]);

        for (int i = 0; i < 8; i++) {
            b[i] = (byte) (r[0] >>> (i * 8));
        }

        for (int i = 0; i < 8; i++) {
            b[i + 8] = (byte) (r[1] >>> (i * 8));
        }
        for (int i = 0; i < 8; i++) {
            b[i + 16] = (byte) (r[2] >>> (i * 8));
        }


        t = new GF2_192(b);
        s = t.toLongArray();
        assertFalse("Fail: constructor on byte array of all 1s", r[0] != s[0] || r[1] != s[1] || r[2]!=s[2]);

        b1 = t.toByteArray();
        assertFalse("Fail: toByteArray all 1s", !Arrays.equals(b, b1));

        b2 = new byte[30];
        for (int i=0; i<24; i++) {
            b2[i+6]=b[i];
        }
        t = new GF2_192(b2, 6);
        s = t.toLongArray();
        assertFalse("Fail: constructor on byte array with offset of all 1s", r[0] != s[0] || r[1] != s[1] || r[2]!=s[2]);

        b1 = t.toByteArray();
        assertFalse("Fail: toByteArray all 1s", !Arrays.equals(b, b1));

        b3 = new byte [40];
        t.toByteArray(b3, 10);
        for (int i = 0; i<b.length; i++) {
            assertFalse("Fail: toByteArray all 1s with offset", b3[i+10]!=b[i]);
        }


    }


    @Test
    public void pow2To2ToKTest() {
        // includes squaring test
        GF2_192 res = new GF2_192();
        GF2_192 z;
        int maxK = 15;

        for (int k=0; k<maxK; k++) {
            GF2_192.power2To2ToK(res, zero,k);
            assertFalse("Fail: power2To2ToK of 0 for k="+k, !res.isZero());

            z = new GF2_192(zero);
            GF2_192.power2To2ToK(z, z,k);
            assertFalse("Fail: power2To2ToK of 0 in place for k="+k, !z.isZero());

            GF2_192.power2To2ToK(res, one, k);
            assertFalse("Fail: power2To2ToK of 1 for k="+k, !res.isOne());

            z = new GF2_192(one);
            GF2_192.power2To2ToK(z, z, k);
            assertFalse("Fail: power2To2ToK of 1 in place for k="+k, !z.isOne());
        }

        GF2_192.sqr(res, zero);
        assertFalse("Fail: sqr of 0", !res.isZero());

        z = new GF2_192(zero);
        GF2_192.sqr(z, z);
        assertFalse("Fail: sqr of 0 in place", !z.isZero());

        GF2_192.sqr(res, one);
        assertFalse("Fail: sqr of 1", !res.isOne());

        z = new GF2_192(one);
        GF2_192.sqr(z, z);
        assertFalse("Fail: sqr of 1 in place", !z.isOne());

        GF2_192 res1 = new GF2_192();
        GF2_192 res2 = new GF2_192();
        for (long[] p : testValues) {
            for (int k=0; k<maxK; k ++) {
                z = new GF2_192(p);
                GF2_192.power2To2ToK(res, z, k);
                if (k==0) {
                    // Ground truth for squaring: self-multiply
                    GF2_192.mul(res1, z, z); // sqr should equal power2To2ToK with k = 0
                    assertFalse("Fail: power2To2To1  " + z, !res.equals(res1));
                    GF2_192.sqr(res2, z); // sqr should equal self-multiply with k = 0
                    assertFalse("Fail: sqr for k = " + k + " value = " + z, !res.equals(res2));
                }
                else {
                    // res1 is the ground truth, computed using smaller values of k than is currently being tested
                    GF2_192.power2To2ToK(res1, res1,k-1);
                    assertFalse("Fail: power2To2ToK for k = " + k + " value = " + z, !res.equals(res1));
                }

                // Input location = output location tests
                GF2_192.power2To2ToK(z, z, k); // power2To2ToK into same location
                assertFalse("Fail: power2To2ToK in place for k = " +k + " value = " + new GF2_192(p), !res.equals(z));
                if (k==0) {
                    z = new GF2_192(p);
                    GF2_192.sqr(z, z); // sqr into same location
                    assertFalse("Fail: sqr in place " + new GF2_192(p), !res.equals(z));

                }
            }
        }
    }

    @Test
    public void specialMultTest() {
        GF2_192 res = new GF2_192();
        GF2t_slow res1 = new GF2t_slow();


        // Run everything times 0 and 0 times everything
        // and everything times 1 and 1 times everything
        // where 0 and 1 are GF2_192

        for (long[] p : testValues) {
            GF2_192 p1 = new GF2_192(p);
            GF2_192.mul(res, p1, zero);
            assertFalse("Fail: " + p1 + " * 0", !res.isZero());
            GF2_192.mul(p1, p1, zero);
            assertFalse("Fail: " + p1 + " * 0" + " in place ", !p1.isZero());
            p1 = new GF2_192(p);
            GF2_192.mul(res, zero, p1);
            assertFalse("Fail: 0 * " + p1, !res.isZero());
            GF2_192.mul(p1, zero, p1);
            assertFalse("Fail: 0 * " + p1 + " in place ", !p1.isZero());
            p1 = new GF2_192(p);
            GF2_192.mul(res, p1, one);
            assertFalse("Fail: " + p1 + " * 1", !res.equals(p1));
            GF2_192.mul(p1, p1, one);
            assertFalse("Fail: " + p1 + " * 1 in place", !res.equals(p1));
            GF2_192.mul(res, one, p1);
            assertFalse("Fail: 1 * " + p1, !res.equals(p1));
            GF2_192.mul(p1, one, p1);
            assertFalse("Fail: 1 * " + p1 + " in place", !res.equals(p1));
        }

        // Run everything times 0
        // and everything times 1
        // where 0 and 1 are bytes
        for (long[] p : testValues) {
            GF2_192 p1 = new GF2_192(p);
            GF2_192.mul(res, p1, (byte) 1);
            assertFalse("Fail: " + p1 + " * 1 byte ", !res.equals(p1));
            GF2_192.mul(p1, p1, (byte) 1);
            assertFalse("Fail: " + p1 + " * 1 byte in place", !res.equals(p1));
            GF2_192.mul(res, p1, (byte) 0);
            assertFalse("Fail: " + p1 + " * 0 byte", !res.isZero());
            GF2_192.mul(p1, p1, (byte) 0);
            assertFalse("Fail: " + p1 + " * 0 byte in place", !p1.isZero());
        }


        // Run everything times every byte
        long[] temp = new long[1];
        for (long[] p : testValues) {
            for (int i = 2; i < 256; i++) {
                GF2_192 p1 = new GF2_192(p);
                temp[0] = i;
                GF2_192.mul(res, p1, (byte) i);
                GF2t_slow.mulBits(res1, p, temp);
                GF2t_slow.modReduce(res1, m);
                assertFalse("Fail: " + p1 + " * " + i + " byte", !res1.equals(res.toLongArray()));
                GF2_192.mul(p1, p1, (byte) i);
                assertFalse("Fail: " + p1 + " * " + i + " byte in place", !res.equals(p1));
            }
        }

    }

    @Test
    public void specialAddTest() {
        GF2_192 res = new GF2_192();

        // Run everything plus 0 and 0 plus everything
        // where 0 is GF2_192


        for (long[] p : testValues) {
            GF2_192 p1 = new GF2_192(p);
            GF2_192.add(res, p1, zero);
            assertFalse("Fail: " + p1 + " + 0", !res.equals(p1));
            GF2_192.add(p1, p1, zero);
            assertFalse("Fail: " + p1 + " + 0 in place", !res.equals(p1));
            GF2_192.add(res, zero, p1);
            assertFalse("Fail: 0 + " + p1, !res.equals(p1));
            GF2_192.add(p1, zero, p1);
            assertFalse("Fail: " + p1 + " + 0 in place", !res.equals(p1));
        }
    }

    @Test
    public void generalAddTest() {
        GF2_192 res = new GF2_192();
        GF2t_slow res1 = new GF2t_slow();
        res1.x = new long[3];


        // Try everything plus everything in the test array
        for (long[] p : testValues) {
            GF2_192 p1 = new GF2_192(p);
            for (long[] q : testValues) {
                GF2_192 q1 = new GF2_192(q);
                GF2_192.add(res, p1, q1);
                res1.x[0] = p[0]^q[0];
                res1.x[1] = p[1]^q[1];
                res1.x[2] = p[2]^q[2];
                assertFalse("Fail: " + p1 + " + " + q1 + " = " + res + " not " + res1, !res1.equals(res.toLongArray()));
                GF2_192.add(p1, p1, q1);
                assertFalse("Fail: " + p1 + " + " + q1 + " in place 1 ", !res.equals(p1));
                p1 = new GF2_192(p);
                GF2_192.add(q1, p1, q1);
                assertFalse("Fail: " + p1 + " + " + q1 + " in place 2 ", !res.equals(q1));
            }
        }

        // Try everything plus self in the test array, both in place and not, and make sure you get zeros
        for (long[] p : testValues) {
            GF2_192 p1 = new GF2_192(p);
            GF2_192.add(res, p1, p1);
            assertFalse("Fail: " + p1 + " + self", !res.isZero());
            GF2_192.add(p1, p1, p1);
            assertFalse("Fail: " + p1 + " self in place", !p1.isZero());
        }

    }


    @Test
    public void generalMultTest() {
        GF2_192 res = new GF2_192();
        GF2t_slow res1 = new GF2t_slow();

        // Now run everything times everything in the test array
        // TODO: speed this up
        for (long[] p : testValues) {
            GF2_192 p1 = new GF2_192(p);
            for (long[] q : testValues) {
                GF2_192 q1 = new GF2_192(q);
                GF2_192.mul(res, p1, q1);
                GF2t_slow.mulBits(res1, p, q);
                GF2t_slow.modReduce(res1, m);
                assertFalse("Fail: " + p1 + " * " + q1, !res1.equals(res.toLongArray()));
                GF2_192.mul(p1, p1, q1);
                assertFalse("Fail: " + p1 + " * " + q1 + " in place 1 ", !res.equals(p1));
                p1 = new GF2_192(p);
                GF2_192.mul(q1, p1, q1);
                assertFalse("Fail: " + p1 + " * " + q1 + " in place 2 ", !res.equals(q1));
            }
        }
        // Try everything times self in the test array, in place
        for (long [] p: testValues) {
            GF2_192 p1 = new GF2_192(p);
            GF2_192.sqr(res, p1);
            GF2_192.mul(p1, p1, p1);
            assertFalse("Fail: " + p1 + " * self in place", !res.equals(p1));
        }

    }

    @Test
    public void inversionTest() {
        GF2_192 res = new GF2_192(), res2 = new GF2_192();
        GF2t_slow res1 = new GF2t_slow();

        // Test inversion of 1
        GF2_192.invert(res, one);
        assertFalse("Fail: inversion of 1", !res.isOne());

        // Test inversion of everything
        for (long[] p : testValues) {
            GF2_192 p1 = new GF2_192(p);
            if (p1.isZero()) continue;
            GF2_192.invert(res, p1);
            GF2_192.mul(res2, p1, res);
            assertFalse("Fail: inversion of " + p1 + " self-test ", !res2.isOne());
            GF2t_slow.mulBits(res1, res.toLongArray(), p);
            GF2t_slow.modReduce(res1, m);
            assertFalse("Fail: inversion of " + p1 + " GF2t_slow-test", !res1.isOne());
            GF2_192.invert(p1, p1);
            assertFalse("Fail: inversion of " + p1 + " in place ", !p1.equals(res));
        }

    }

    @Test
    public void interpolateTest() {

        // Test for null inputs, arrays of unequal length, etc.
        GF2_192[] optArray = new GF2_192[2];
        optArray[0] = null;
        optArray[1] = new GF2_192(17);

        GF2_192_Poly res;


        Random rand = new Random();

        for (int len = 1; len < 100; len++) {
            byte[] points = new byte[len];
            GF2_192[] values = new GF2_192[len];
            byte[] temp = new byte[24];
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
                values[i] = new GF2_192(temp);
            }

            res = GF2_192_Poly.interpolate(points, values, null);
            for (int i = 0; i < len; i++) {
                GF2_192 t = res.evaluate(points[i]);
                assertFalse("Interpolation error on length = " + len + " at input point number " + i, !t.equals(values[i]));
            }
            rand.nextBytes(temp);
            GF2_192 valueAt0 = new GF2_192(temp);
            res = GF2_192_Poly.interpolate(points, values, valueAt0);
            for (int i = 0; i < len; i++) {
                GF2_192 t = res.evaluate(points[i]);
                assertFalse("Interpolation error on length =  " + len + " at input point number " + i + "(with optional 0)", !t.equals(values[i]));

            }
            GF2_192 t = res.evaluate((byte) 0);
            assertFalse("Interpolation error on length =  " + len + " at input optional 0", !t.equals(valueAt0));

            byte [] b = res.toByteArray(false);
            GF2_192_Poly t1 = GF2_192_Poly.fromByteArray(valueAt0.toByteArray(), b);
            byte [] b1 = t1.toByteArray(false);
            assertFalse("To byte array round trip error " + Arrays.toString(b)+" "+Arrays.toString(b1), !Arrays.equals(b, b1));
            byte [] b2 = t1.toByteArray(true);
            assertFalse("To byte array round trip error at coeff0", !Arrays.equals(valueAt0.toByteArray(), Arrays.copyOfRange(b2, 0, 24)));
            assertFalse("To byte array round trip error with coeff0 at later coeff", !Arrays.equals(b1, Arrays.copyOfRange(b2, 24, b2.length)));
            byte [] b3 = t1.coeff0Bytes();
            assertFalse("To byte array round trip error on coeff0", !Arrays.equals(b3, valueAt0.toByteArray()));
        }

        for (int len = 1; len < 100; len++) {
            byte[] points = new byte[len];
            GF2_192[] values = new GF2_192[len];
            byte[] temp = new byte[24];

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
                values[i] = new GF2_192(temp);
            }

            res = GF2_192_Poly.interpolate(points, values, null);
            for (int i = 0; i < len; i++) {
                GF2_192 t = res.evaluate(points[i]);
                assertFalse("Interpolation error on length =  " + len + " " + i + "(with 0 allowed but not additional)", !t.equals(values[i]));
            }

            for (GF2_192 opt : optArray) {
                res = GF2_192_Poly.interpolate(null, values, opt);
                assertFalse("Fail: interpolate should output null on points = null", res != null);
                res = GF2_192_Poly.interpolate(points, null, opt);
                assertFalse("Fail: interpolate should output null on values =  null", res != null);
                res = GF2_192_Poly.interpolate(points, new GF2_192[0], opt);
                assertFalse("Fail: interpolate should output null on values of length 0", res != null);
                res = GF2_192_Poly.interpolate(new byte[0], values, opt);
                assertFalse("Fail: interpolate should output null on points of length 0", res != null);
                res = GF2_192_Poly.interpolate(new byte[len - 1], values, opt);
                assertFalse("Fail: interpolate should output null on not enough points", res != null);
                res = GF2_192_Poly.interpolate(new byte[len + 1], values, opt);
                assertFalse("Fail: interpolate should output null on too many points", res != null);
            }
        }

        for (GF2_192 opt : optArray) {
            res = GF2_192_Poly.interpolate(null, null, opt);
            assertFalse("Fail: interpolate should output null on both points and values = null", res != null);
        }
    }
}
