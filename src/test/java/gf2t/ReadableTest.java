package gf2t;

import com.google.common.base.Joiner;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;

import java.util.LinkedList;
import java.util.stream.Collectors;

public class ReadableTest extends BlockJUnit4ClassRunner {

    @Override
    protected String testName(FrameworkMethod method) {
        Joiner j = Joiner.on(" ");
        return j.join(
                splitCamelCaseString(method.getMethod().getName())
                        .stream()
                        .map((s) -> s.substring(0, 1).toUpperCase() + s.substring(1)).collect(Collectors.toList())
        );
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