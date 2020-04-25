package gf2t;

import com.google.common.base.Joiner;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;

import java.util.LinkedList;

public class ReadableTest extends BlockJUnit4ClassRunner {

    @Override
    protected String testName(FrameworkMethod method) {
        Joiner j = Joiner.on(" ");
        String[] parts = splitCamelCaseString(method.getMethod().getName()).toArray(new String[0]);
        String[] mapped = new String[parts.length];
        for (int i = 0; i < parts.length; i++) {
           String s = parts[i];
           mapped[i] = s.substring(0, 1).toUpperCase() + s.substring(1);
        }
        return j.join(mapped);
    }

    public ReadableTest(Class<?> klass) throws InitializationError {
        super(klass);
    }

    public static LinkedList<String> splitCamelCaseString(String s) {
        LinkedList<String> result = new LinkedList<String>();
        for (String w : s.split("(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])")) {
            result.add(w);
        }
        return result;
    }


}