package edu.njit.cs631citylib;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

public class TopNBWin extends JDialog {

	private JTextField txtN;
	private JTextField txtI;
	
	public static void main(String[] args) {
		try {
			TopNBWin dialog = new TopNBWin();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public TopNBWin(){
	
		setTitle("CITY LIBRARY");
	getContentPane().setBackground(new Color(255,250,250));
	//Connect to Database
	Database m = Database.getInstance();
	m.connect();

	setBounds(200, 200, 450, 400);
	getContentPane().setLayout(null);
	
	 
		
		JLabel lblTopNB = new JLabel("Fetch Top N Most Frequent Borrowers");
		lblTopNB.setBounds(55, 78, 250, 31);
		getContentPane().add(lblTopNB);
		
		JLabel lblN = new JLabel("Enter Value for N:");
		lblN.setBounds(38, 149, 120, 30);
		getContentPane().add(lblN);
		
		txtN = new JTextField();
		txtN .setBounds(150, 149, 130, 30);
		getContentPane().add(txtN);
		txtN.setColumns(10);
		
		JLabel lblI = new JLabel("Enter Branch ID:");
		lblI.setBounds(38, 179, 120, 30);
		getContentPane().add(lblI);
		
		txtI = new JTextField();
		txtI.setBounds(150, 179, 130, 30);
		getContentPane().add(txtI);
		txtI.setColumns(10);
		
		/*
		 * JLabel lblLID = new JLabel("BID"); lblLID.setBounds(38, 229, 90, 16);
		 * getContentPane().add(lblLID);
		 * 
		 * txtLID = new JTextField(); txtLID.setBounds(125, 229, 130, 26);
		 * getContentPane().add(txtLID); txtLID.setColumns(10);
		 */
		
	
	/*JLabel lblNewLabel = new JLabel("City Library");
	lblNewLabel.setFont(new Font("Lucida Grande", Font.PLAIN, 16));
	lblNewLabel.setForeground(Color.BLACK);
	lblNewLabel.setBounds(202, 24, 181, 50);
	getContentPane().add(lblNewLabel);*/
	
	JButton btnSearch = new JButton("Search");
	btnSearch.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			
			if (txtN.getText().length() <= 0) {
				JOptionPane.showMessageDialog(null, "Enter Value For N");
				return;
			}
			if (txtI.getText().length() <= 0) {
				JOptionPane.showMessageDialog(null, "Enter Branch ID");
				return;
			}
			/*
			 * if (txtLID.getText().length() <= 0) { JOptionPane.showMessageDialog(null,
			 * "Enter BID"); return; }
			 */

			
			TopReaders dialog = new TopReaders(txtN.getText(),txtI.getText());
			dialog.setModal(true);
			dialog.setVisible(true);
		}
	});

	btnSearch.setBounds(150, 209, 100, 29);
	getContentPane().add(btnSearch);
	

}
	
}
