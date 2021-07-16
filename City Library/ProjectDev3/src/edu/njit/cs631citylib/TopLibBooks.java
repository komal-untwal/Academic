package edu.njit.cs631citylib;


import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

public class TopLibBooks extends JDialog {

	private JTextField txtN;
	
	
	public static void main(String[] args) {
		try {
			TopBooks dialog = new TopBooks();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public TopLibBooks(){
	setTitle("CITY LIBRARY");
	getContentPane().setBackground(new Color(255,250,250));
	//Connect to Database
	Database m = Database.getInstance();
	m.connect();

	setBounds(200, 200, 450, 400);
	getContentPane().setLayout(null);
	
	 
		
		JLabel lblTopNB = new JLabel("Top N Borrowed Books from Library");
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
			
			
			TopBorrow dialog = new TopBorrow(txtN.getText());
			dialog.setModal(true);
			dialog.setVisible(true);
		}
	});

	btnSearch.setBounds(150, 199, 100, 29);
	getContentPane().add(btnSearch);
	

}
	
}
