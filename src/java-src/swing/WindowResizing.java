package hrtime.swing;

import javax.swing.*;

public class WindowResizing implements WindowResizeListener {
    public static void main(String[] args) {
	JFrame f = new JFrame();
	f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	f.setSize(100, 100);

	WindowResizeListener l = new WindowResizing();
	WindowResizeMonitor.register(f, l);

	f.setVisible(true);
    }

    public void windowResized(WindowResizeEvent e) {
	System.out.println(e);
    }
}
