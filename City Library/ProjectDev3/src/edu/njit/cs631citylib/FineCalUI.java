package edu.njit.cs631citylib;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Date;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

public class FineCalUI extends JDialog {

	private JTextField txtN;
	private JTextField txtI;
	
	public static void main(String[] args) {
		try {
			FineCalUI dialog = new FineCalUI();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public FineCalUI(){
		setTitle("CITY LIBRARY");
	
	getContentPane().setBackground(new Color(255,250,250));
	//Connect to Database
	Database m = Database.getInstance();
	m.connect();

	setBounds(200, 200, 500, 550);
	getContentPane().setLayout(null);
	
	 
		
		JLabel lblTopNB = new JLabel("Average Fine Paid by borrowers");
		lblTopNB.setBounds(74, 78, 250, 31);
		getContentPane().add(lblTopNB);
		
		JLabel lblN = new JLabel("Start Date:");
		lblN.setBounds(38, 149, 120, 30);
		getContentPane().add(lblN);
		
		txtN = new JTextField();
		txtN .setBounds(150, 149, 130, 30);
		getContentPane().add(txtN);
		txtN.setColumns(10);
		
		JLabel lblI = new JLabel("End Date:");
		lblI.setBounds(38, 179, 120, 30);
		getContentPane().add(lblI);
		
		txtI = new JTextField();
		txtI.setBounds(150, 179, 130, 30);
		getContentPane().add(txtI);
		txtI.setColumns(10);
		
		
	JButton btnSearch = new JButton("Search");
	btnSearch.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			
			if (txtN.getText().length() <= 0) {
				JOptionPane.showMessageDialog(null, "Enter Value For Start Date");
				return;
			}
			if (txtI.getText().length() <= 0) {
				JOptionPane.showMessageDialog(null, "Enter Value for End Date");
				return;
			}
			
			AvgFine dialog = new AvgFine(txtN.getText(),txtI.getText());
			dialog.setModal(true);
			dialog.setVisible(true);
		}
	});

	btnSearch.setBounds(150,209,100, 29);
	getContentPane().add(btnSearch);
	

}
	
}
