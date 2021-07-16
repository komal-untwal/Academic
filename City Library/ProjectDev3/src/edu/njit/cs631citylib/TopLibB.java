package edu.njit.cs631citylib;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

public class TopLibB extends JDialog {

	private JTextField txtN;
	//private JTextField txtI;
	
	public static void main(String[] args) {
		try {
			TopLibB dialog = new TopLibB();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public TopLibB(){
	setTitle("CITY LIBRARY");
	getContentPane().setBackground(new Color(255,250,250));
	//Connect to Database
	Database m = Database.getInstance();
	m.connect();

	setBounds(200, 200, 450, 400);
	getContentPane().setLayout(null);
	
	 
		
		JLabel lblTopNB = new JLabel("Fetch Top N Most Frequent Borrowers");
		lblTopNB.setBounds(54, 78, 250, 31);
		getContentPane().add(lblTopNB);
		
		JLabel lblN = new JLabel("Enter Value for N:");
		lblN.setBounds(38, 149, 120, 30);
		getContentPane().add(lblN);
		
		txtN = new JTextField();
		txtN .setBounds(150, 149, 130, 30);
		getContentPane().add(txtN);
		txtN.setColumns(10);
		
		
	
	JButton btnSearch = new JButton("Search");
	btnSearch.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			
			if (txtN.getText().length() <= 0) {
				JOptionPane.showMessageDialog(null, "Enter Value For N");
				return;
			}
		

			
			TopReaders dialog = new TopReaders(txtN.getText());
			dialog.setModal(true);
			dialog.setVisible(true);
		}
	});

	btnSearch.setBounds(150, 179, 100, 30);
	getContentPane().add(btnSearch);
	

}
	
}