package gf2t;

import java.util.Optional;

public class GF2_128_Poly {
    private GF2_128 [] c; // must be not null and of length at least 1

    private int deg; // must be >=0. actual degree is <= deg. c[deg+1]...c[c.length-1] must be 0 or null
    // deg of the 0 polynomial is 0


    /**
     * Interpolates the polynomial at given points (and at point 0, if valueAt0.isPresent()).
     * If points are not all distinct, or if 0 is in the points array and valueAt0.isPresent(), behavior is undefined.
     * valueAt0 is separated only for efficiency reason; the caller can treat 0 like any other point instead
     * (i.e., the points array can include 0 if !valueAt0.isPresent(), but computation will be slightly less efficient).
     * If points or values is null, or if their lengths differ, returns null.
     *
     * @param points the set of distinct inputs to the returned polynomial
     *               (last byte of the field element only; all other bits are assumed to be 0)
     * @param values value[i] will be the result evaluating the returned polynomial at points[i]. Values[i] must not be null.
     * @param valueAt0 if isPresent(), then valueAt0.get() will be the result of evaluating the returned polynomial at 0
     * @return the unique lowest-degree polynomial p such that for every i, p(points[i]) = values[i] and p(0)=valueAt0.get()
     *         (if valueAt0.isPresent())
     */
    public static GF2_128_Poly interpolate (byte[] points, GF2_128 [] values, Optional<GF2_128> valueAt0) {
        if (points == null || values == null || values.length == 0 || values.length!=points.length) return null;
        boolean zeroPresent = valueAt0.isPresent();

        int resultDegree = values.length-1;
        if (zeroPresent) {
            resultDegree++;
        }

        GF2_128_Poly result = new GF2_128_Poly(resultDegree,  0);
        GF2_128_Poly vanishingPoly = new GF2_128_Poly(resultDegree, 1);

        for (int i = 0; i < points.length; i++) {
            GF2_128 t = result.evaluate(points[i]);
            GF2_128 s = vanishingPoly.evaluate(points[i]);

            // need to find r such that currentValue+r*valueOfVanshingPoly = values[i]
            GF2_128.add(t, t, values[i]);
            GF2_128.invert(s, s);
            GF2_128.mul(t, t, s);

            result.addMonicTimesConstantTo(vanishingPoly, t);

            if (i < points.length - 1 || zeroPresent) {
                vanishingPoly.monicTimesMonomial(points[i]);
            }
        }

        if (zeroPresent) { // the last point is 0
            GF2_128 t = new GF2_128(result.c[0]); // evaluating at 0 is easy
            GF2_128 s = new GF2_128(vanishingPoly.c[0]); // evaluating at 0 is easy

            // need to find r such that currentValue+r*valueOfVanshingPoly = valueAt0]
            GF2_128.add(t, t, valueAt0.get());
            GF2_128.invert(s, s);
            GF2_128.mul(t, t, s);
            result.addMonicTimesConstantTo(vanishingPoly, t);
        }
        return result;
    }

    /**
     * Evaluates the polynomial at a given point
     * @param x the last byte of a field element (all other bits are assumed to be 0)
     * @return the value of this polynomial evaluated at the field element
     */
    public GF2_128 evaluate (byte x) {
        GF2_128 res = new GF2_128(c[deg]);
        for (int d = deg-1; d>=0; d--) {
            GF2_128.mul(res, res, x);
            GF2_128.add(res, res, c[d]);
        }
        return res;
    }

    /**
     * adds r*p to this; assumes p is monic, c.length>p.deg, and (p.deg == this.deg+1, or this==0 and p==1)
     * @param p the monic polynomial being added to this
     * @param r the constant by which p is multiplied before being added
     */
    private void addMonicTimesConstantTo (GF2_128_Poly p, GF2_128 r) {
        GF2_128 t = new GF2_128();
        for (int i = 0; i<p.deg; i++) {
            GF2_128.mul (t, p.c[i], r);
            GF2_128.add (c[i], c[i], t);
        }
        deg = p.deg;
        c[deg] = new GF2_128(r);
    }


    /**
     * multiplies this by (x+r), assuming this is monic of degree deg (i.e. assumed c[deg]==1)
     * @param r the constant term of the monomial
     */
    private void monicTimesMonomial (byte r) {
        deg++;
        c[deg] = new GF2_128(1);
        for (int i = deg - 1; i > 0; i--) {
            // c[i] = c[i-1]+r*c[i]
            GF2_128.mul(c[i], c[i], r);
            GF2_128.add(c[i], c[i], c[i - 1]);
        }
        GF2_128.mul(c[0], c[0], r);
    }


    /**
     * Constructs a constant polynomial
     *
     * @param maxDeg the maximum degree this polynomial could possibly have (to allocate space)
     * @param constantTerm the polynomial is initially created with degree 0 and given constantTerm
     */
    private GF2_128_Poly (int maxDeg, int constantTerm) {
        c = new GF2_128[maxDeg+1];
        c[0] = new GF2_128(constantTerm);
        deg = 0;
    }

    /**
     *
     * @return this represented in usual polynomial notation (but possibly leading 0s), with X as the free variable
     */
    public String toString() {
        String ret = "";
        if (deg>=2) {
            ret+= c[deg].toString() + "*X^"+deg;
            int i;
            for (i = deg - 1; i >= 2; i--) {
                ret += " + " + c[i]+"*X^"+i;
            }
            ret+= " + ";
        }
        if (deg>=1) {
            ret += c[1] + "*X" + " + ";
        }
        ret+=c[0];
        return ret;
    }
}
