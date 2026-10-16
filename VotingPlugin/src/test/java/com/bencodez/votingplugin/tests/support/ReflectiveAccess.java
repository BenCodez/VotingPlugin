package com.bencodez.votingplugin.tests.support;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Proxy;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/** Reflection plumbing for typed test views of non-public production seams. */
public final class ReflectiveAccess {
    private ReflectiveAccess() { }

    // A callback has one stable production identity per native interface for this test JVM.
    private static final Map<Object, Map<Class<?>, Object>> CALLBACK_VIEWS = new java.util.IdentityHashMap<>();


    public interface View { Object delegate(); }

    @SuppressWarnings("unchecked")
    public static <T> Class<T> type(String name) {
        try {
            return (Class<T>) Class.forName(name, false, ReflectiveAccess.class.getClassLoader());
        } catch (ClassNotFoundException failure) {
            throw new AssertionError("Missing production test seam: " + name, failure);
        }
    }

    public static Object construct(String owner, String[] signature, Object[] arguments) {
        try {
            Constructor<?> constructor = Arrays.stream(type(owner).getDeclaredConstructors())
                    .filter(candidate -> matches(candidate.getParameterTypes(), signature))
                    .findFirst().orElseThrow(() -> new NoSuchMethodException(owner + Arrays.toString(signature)));
            constructor.setAccessible(true);
            return constructor.newInstance(adaptArguments(arguments, constructor.getGenericParameterTypes()));
        } catch (InvocationTargetException failure) {
            return raise(failure.getCause());
        } catch (ReflectiveOperationException failure) {
            throw new AssertionError("Cannot construct production test seam: " + owner, failure);
        }
    }

    public static Object call(String owner, Object receiver, String name, String[] signature, Object[] arguments) {
        try {
            Method method = Arrays.stream(type(owner).getDeclaredMethods())
                    .filter(candidate -> candidate.getName().equals(name)
                            && matches(candidate.getParameterTypes(), signature))
                    .findFirst().orElseThrow(() -> new NoSuchMethodException(owner + "." + name + Arrays.toString(signature)));
            method.setAccessible(true);
            return method.invoke(unwrap(receiver), adaptArguments(arguments, method.getGenericParameterTypes()));
        } catch (InvocationTargetException failure) {
            return raise(failure.getCause());
        } catch (ReflectiveOperationException failure) {
            throw new AssertionError("Cannot invoke production test seam: " + owner + "." + name, failure);
        }
    }

    public static void set(java.lang.reflect.Field field, Object receiver, Object value) throws IllegalAccessException {
        field.set(unwrap(receiver), adapt(value, field.getGenericType()));
    }

    public static Object invoke(Method method, Object receiver, Object[] arguments)
            throws IllegalAccessException, InvocationTargetException {
        return method.invoke(unwrap(receiver), adaptArguments(arguments, method.getGenericParameterTypes()));
    }

    public static <T> T newInstance(Constructor<T> constructor, Object[] arguments) throws ReflectiveOperationException {
        return constructor.newInstance(adaptArguments(arguments, constructor.getGenericParameterTypes()));
    }

    public static Object field(String owner, String name) { return field(owner, null, name); }

    public static Object field(String owner, Object receiver, String name) {
        try {
            java.lang.reflect.Field field = type(owner).getDeclaredField(name);
            field.setAccessible(true);
            return field.get(unwrap(receiver));
        } catch (ReflectiveOperationException failure) {
            throw new AssertionError("Cannot read production test seam: " + owner + "." + name, failure);
        }
    }

    public static <T extends Enum<T>> T enumView(Class<T> type, Object value) {
        return value == null ? null : Enum.valueOf(type, ((Enum<?>) value).name());
    }

    public static Object defaultCall(Object callback, String owner, String name, String[] signature, Object[] arguments) {
        return call(owner, adapt(callback, type(owner)), name, signature, arguments);
    }

    public static <K> Map<K, Object> nativeValues(Map<K, ? extends View> values) {
        Map<K, Object> result = new LinkedHashMap<>();
        values.forEach((key, value) -> result.put(key, unwrap(value)));
        return result;
    }

    public static Object unwrap(Object value) { return value instanceof View view ? view.delegate() : value; }

    /** Exposes the original callback for identity assertions across the access boundary. */
    public static Object callbackTarget(Object value) {
        if (value != null && Proxy.isProxyClass(value.getClass())
                && Proxy.getInvocationHandler(value) instanceof Callback callback) return callback.target();
        return value;
    }

    private static boolean matches(Class<?>[] parameters, String[] signature) {
        if (parameters.length != signature.length) return false;
        for (int index = 0; index < parameters.length; index++) {
            if (!parameters[index].getSimpleName().equals(signature[index])) return false;
        }
        return true;
    }

    private static Object[] adaptArguments(Object[] arguments, Type[] expected) {
        Object[] adapted = new Object[arguments.length];
        for (int index = 0; index < arguments.length; index++) adapted[index] = adapt(arguments[index], expected[index]);
        return adapted;
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    private static Object adapt(Object value, Type expectedType) {
        value = unwrap(value);
        if (value == null) return null;
        Class<?> expected = expectedType instanceof ParameterizedType generic
                ? (Class<?>) generic.getRawType() : expectedType instanceof Class<?> concrete ? concrete : Object.class;
        if (expected.isEnum() && value instanceof Enum<?> enumeration) {
            return Enum.valueOf((Class) expected, enumeration.name());
        }
        if (expectedType instanceof ParameterizedType generic) {
            Type[] parameters = generic.getActualTypeArguments();
            if (value instanceof Map<?, ?> map && Map.class.isAssignableFrom(expected)) {
                Map<Object, Object> result = new LinkedHashMap<>();
                boolean changed = false;
                for (Map.Entry<?, ?> entry : map.entrySet()) {
                    Object key = adapt(entry.getKey(), parameters[0]);
                    Object item = adapt(entry.getValue(), parameters[1]);
                    changed |= key != entry.getKey() || item != entry.getValue();
                    result.put(key, item);
                }
                return changed ? result : value;
            }
            if (value instanceof Collection<?> entries && Collection.class.isAssignableFrom(expected)) {
                List<Object> result = entries.stream().map(entry -> adapt(entry, parameters[0])).toList();
                boolean changed = false;
                int index = 0;
                for (Object entry : entries) changed |= entry != result.get(index++);
                return !changed ? value : Set.class.isAssignableFrom(expected) ? result.stream().collect(Collectors.toSet()) : result;
            }
        }
        if (View.class.isAssignableFrom(expected) && !expected.isInstance(value)) {
            try {
                return expected.getMethod("view", Object.class).invoke(null, value);
            } catch (ReflectiveOperationException failure) {
                throw new AssertionError("Cannot create typed test view: " + expected.getName(), failure);
            }
        }
        if (expected.isInterface() && !expected.isInstance(value)) {
            synchronized (CALLBACK_VIEWS) {
                Map<Class<?>, Object> views = CALLBACK_VIEWS.computeIfAbsent(value, ignored -> new java.util.HashMap<>());
                Object callback = value;
                return views.computeIfAbsent(expected, type -> Proxy.newProxyInstance(type.getClassLoader(),
                        new Class<?>[]{type}, new Callback(callback)));
            }
        }
        return value;
    }

    private record Callback(Object target) implements java.lang.reflect.InvocationHandler {
        @Override public Object invoke(Object proxy, Method method, Object[] arguments) throws Throwable {
            if (method.getDeclaringClass() == Object.class) {
                return switch (method.getName()) {
                    case "toString" -> target.toString();
                    case "hashCode" -> System.identityHashCode(proxy);
                    case "equals" -> proxy == arguments[0];
                    default -> throw new AssertionError(method);
                };
            }
            Method callback = Arrays.stream(target.getClass().getMethods())
                    .filter(candidate -> candidate.getName().equals(method.getName())
                            && candidate.getParameterCount() == method.getParameterCount())
                    .findFirst().orElseThrow(() -> new AssertionError("Missing test callback: " + method.getName()));
            callback.setAccessible(true);
            if (method.isDefault() && callback.isDefault()) {
                Object[] input = arguments == null ? new Object[0] : arguments;
                return java.lang.invoke.MethodHandles.privateLookupIn(method.getDeclaringClass(),
                        java.lang.invoke.MethodHandles.lookup()).unreflectSpecial(method, method.getDeclaringClass())
                        .bindTo(proxy).invokeWithArguments(input);
            }
            try {
                Object[] input = arguments == null ? new Object[0] : arguments;
                return adapt(callback.invoke(target, adaptArguments(input, callback.getGenericParameterTypes())),
                        method.getGenericReturnType());
            } catch (InvocationTargetException failure) {
                throw failure.getCause();
            }
        }
    }

    @SuppressWarnings("unchecked")
    private static <T, E extends Throwable> T raise(Throwable failure) throws E { throw (E) failure; }
}
