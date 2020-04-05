package hrtime.swing;
import java.util.EventObject;
import java.awt.Dimension;

public class WindowResizeEvent extends EventObject {
    private Dimension oldSize;
    private Dimension newSize;

    public WindowResizeEvent(Object source, Dimension oldSize,
			     Dimension newSize) {
	super(source);
	this.oldSize = oldSize;
	this.newSize = newSize;
    }

    public Dimension getOldSize() {
	return oldSize;
    }

    public Dimension getNewSize() {
	return newSize;
    }
}
