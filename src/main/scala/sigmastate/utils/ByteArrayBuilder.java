package sigmastate.utils;

import java.nio.ByteBuffer;
import java.util.Arrays;

/** Similar to StringBuilder but works with underlying Array[Byte].
* Borrowed from https://github.com/odnoklassniki/one-nio/blob/master/src/one/nio/util/ByteArrayBuilder.java
* Modifications of the underlying array is performed via ByteBuffer wrapper, so that saved bytes can
* be read back via ByteBuffer API. */
public class ByteArrayBuilder {

    protected byte[] arr;
    protected ByteBuffer buf;

    public ByteArrayBuilder() {
        this(256);
    }

    public ByteArrayBuilder(int capacity) {
        this.arr = new byte[capacity];
        this.buf = ByteBuffer.wrap(this.arr);
    }

    public final byte[] array() {
        return arr;
    }

    public final int length() {
        return buf.position();
    }

    public final void setLength(int newPosition) {
        buf.position(newPosition);
    }

    public final int capacity() {
        return arr.length;
    }

    public final byte byteAt(int index) {
        return arr[index];
    }

    public final void crop(int offset) {
        int count = buf.position();
        if (offset < count) {
            count -= offset;
            System.arraycopy(arr, offset, arr, 0, count);
        } else {
            count = 0;
        }
    }

    public final byte[] trim() {
        int count = buf.position();
        if (arr.length > count) {
            arr = Arrays.copyOf(arr, count);
            buf = ByteBuffer.wrap(arr);
        }
        return arr;
    }

    public final byte[] toBytes() {
        int count = buf.position();
        byte[] result = new byte[count];
        System.arraycopy(arr, 0, result, 0, count);
        return result;
    }

    public final ByteArrayBuilder append(byte b) {
        ensureCapacity(1);
        buf.put(b);
        return this;
    }

    public final ByteArrayBuilder append(byte[] b) {
        return append(b, 0, b.length);
    }

    public final ByteArrayBuilder append(byte[] b, int offset, int length) {
        ensureCapacity(length);
        buf.put(b, offset, length);
        return this;
    }

    public final ByteArrayBuilder append(boolean b) {
        append((byte)(b ? 0x01 : 0x00));
        return this;
    }

    public final ByteArrayBuilder append(char c) {
        ensureCapacity(1);
        buf.putChar(c);
        return this;
    }

    public final ByteArrayBuilder append(short n) {
        ensureCapacity(2);
        buf.putShort(n);
        return this;
    }

    public final ByteArrayBuilder append(int n) {
        ensureCapacity(4);
        buf.putInt(n);
        return this;
    }

    public final ByteArrayBuilder append(long n) {
        ensureCapacity(8);
        buf.putLong(n);
        return this;
    }

    private void ensureCapacity(int required) {
        int count = buf.position();
        if (count + required > arr.length) {
            arr = Arrays.copyOf(arr, Math.max(count + required, arr.length << 1));
            newBuffer(arr);
        }
    }

    private void newBuffer(byte[] newArr) {
        ByteBuffer newBuf = ByteBuffer.wrap(newArr);
        newBuf.position(buf.position());
        buf = newBuf;
    }

}
