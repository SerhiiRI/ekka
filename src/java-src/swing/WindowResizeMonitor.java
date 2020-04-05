package hrtime.swing;
import java.util.*;
import java.awt.Window;
import java.awt.Dimension;

public class WindowResizeMonitor implements Runnable {
    private static final HashMap<Window, WindowResizeMonitor> WINDOW_MAP = new HashMap<Window, WindowResizeMonitor>();

    private List<WindowResizeListener> listeners = new ArrayList<WindowResizeListener>();
    private boolean run = false;
    private Window window;

    private WindowResizeMonitor(Window window) {
	this.window = window;
    }

    public static void register(Window window, WindowResizeListener listener) {
	WindowResizeMonitor monitor = (WindowResizeMonitor)
	    WINDOW_MAP.get(window);

	if (monitor == null) {
	    monitor = new WindowResizeMonitor(window);
	    WINDOW_MAP.put(window, monitor);
	}
	monitor.add(listener);
    }

    public static void unregister(Window window, WindowResizeListener
				  listener) {
	WindowResizeMonitor monitor = (WindowResizeMonitor)
	    WINDOW_MAP.get(window);

	if (monitor != null) {
	    monitor.remove(listener);
	}
    }

    private synchronized void add(WindowResizeListener listener) {
	listeners.add(listener);

	if (!run) {
	    run = true;
	    new Thread(this).start();
	}
    }

    private synchronized void remove(WindowResizeListener listener) {
	listeners.remove(listener);

	if (run && listeners.isEmpty()) {
	    run = false;
	}
    }

    public void run() {
	Dimension oldSize = window.getSize();

	try {
	    while (run) {
		Thread.sleep(100);

		Dimension curSize = window.getSize();
		if (!oldSize.equals(curSize)) {
		    fireWindowResizeEvent(new WindowResizeEvent(window,
								oldSize, curSize));

		    oldSize = curSize;
		}
	    }
	} catch (InterruptedException e) {
	}
    }

    private void fireWindowResizeEvent(WindowResizeEvent event) {
	Iterator it = listeners.iterator();

	while (it.hasNext()) {
	    WindowResizeListener l = (WindowResizeListener) it.next();
	    l.windowResized(event);
	}
    }
}
