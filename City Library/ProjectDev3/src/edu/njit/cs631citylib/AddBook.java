package edu.njit.cs631citylib;

import java.awt.*;
import java.awt.event.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.*;


public class AddBook extends JDialog{
	
	public static void main(String[] args) {
		try {
			AddBook dialog = new AddBook();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
public AddBook() {
		
		
        setTitle("CITY LIBRARY");
		
        getContentPane().setBackground(new Color(255,250,250));
		//Connect to Database
		Database m = Database.getInstance();
		m.connect();
	
		setBounds(100, 100, 500, 700);
		getContentPane().setLayout(null);
		
		JLabel lblAddBook = new JLabel("Please Enter Book Details:");
		lblAddBook.setBounds(74, 78, 200, 31);
		getContentPane().add(lblAddBook);
		
		JLabel lblBookID = new JLabel("Book Id:");
		lblBookID.setBounds(38, 149, 90, 16);
		getContentPane().add(lblBookID);
		
		JTextField txtBookID = new JTextField();
		txtBookID.setBounds(200, 149, 130, 26);
		getContentPane().add(txtBookID);
		txtBookID.setColumns(10);
		
		JLabel lblISBN = new JLabel("ISBN: ");
		lblISBN.setBounds(38, 199, 90, 16);
		getContentPane().add(lblISBN);
		
		JTextField txtISBN = new JTextField();
		txtISBN.setBounds(200, 199, 130, 26);
		getContentPane().add(txtISBN);
		txtISBN.setColumns(10);
		
		JLabel lblTitle = new JLabel("Name:");
		lblTitle.setBounds(38, 249, 90, 16);
		getContentPane().add(lblTitle);
		
		JTextField txtTitle = new JTextField();
		txtTitle.setBounds(200, 249, 130, 26);
		getContentPane().add(txtTitle);
		txtTitle.setColumns(10);
		
		JLabel lblPDate = new JLabel("Date of Publication:");
		lblPDate.setBounds(38, 299, 130, 16);
		getContentPane().add(lblPDate);
		
		JTextField txtPDate = new JTextField();
		txtPDate.setBounds(200, 299, 130, 26);
		getContentPane().add(txtPDate);
		txtPDate.setColumns(10);
		
		JLabel lblPID = new JLabel("Publisher Id:");
		lblPID.setBounds(38, 349, 90, 16);
		getContentPane().add(lblPID);
		
		JTextField txtPID = new JTextField();
		txtPID.setBounds(200, 349, 130, 26);
		getContentPane().add(txtPID);
		txtPID.setColumns(10);
		
		JLabel lblLID = new JLabel("Branch Id:");
		lblLID.setBounds(38, 399, 90, 16);
		getContentPane().add(lblLID);
		
		JTextField txtLID = new JTextField();
		txtLID.setBounds(200, 399, 130, 26);
		getContentPane().add(txtLID);
		txtLID.setColumns(10);
		
		JLabel lblPos = new JLabel("Position:");
		lblPos.setBounds(38, 449, 90, 16);
		getContentPane().add(lblPos);
		
		JTextField txtPos = new JTextField();
		txtPos.setBounds(200, 449, 130, 26);
		getContentPane().add(txtPos);
		txtPos.setColumns(10);
		
		JButton btnAddBook = new JButton("Submit");
		btnAddBook.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (txtBookID.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please type ID");
					return;
				}
				if (txtISBN.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please specify ISBN");
					return;
				}
				if (txtTitle.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please specify name");
					return;
				}
				if (txtLID.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please specify Branch");
					return;
				}
				
					
				String id = txtBookID.getText();
				Integer idi = Integer.parseInt(id);
				String lid = txtLID.getText();
				Integer idl = Integer.parseInt(lid);
				String pid = txtPID.getText();
				Integer idp = Integer.parseInt(pid);
				//String ty = txtReaderType.getText();
				//String nm = txtReaderName.getText();
				//String ad = txtReaderAdd.getText();
				
				
				ArrayList<ArrayList<Object>> resultl = m.execQuery("SELECT * FROM `BRANCH` WHERE BID = '" + txtLID.getText() + "';");
				if(resultl.size()==0){
					
						JOptionPane.showMessageDialog(null, "No BRANCH  WITH THIS ID. CANNOT INSERT");
					
					
				}
				ArrayList<ArrayList<Object>> resultp = m.execQuery("SELECT * FROM `PUBLISHER` WHERE PUBLISHERID = '" + txtPID.getText() + "';");
				if(resultp.size()==0){
					
						JOptionPane.showMessageDialog(null, "No PUBLISHER  WITH THIS ID. CANNOT INSERT");
					
					
				}
				ArrayList<ArrayList<Object>> result = m.execQuery("SELECT * FROM `DOCUMENT` WHERE DOCID = '" + txtBookID.getText() + "';");
				if(result.size() ==0){
			
				int afr1 = m.execUpdate("INSERT INTO DOCUMENT (DOCID, TITLE, PDATE, PUBLISHERID) "
				          +"VALUES ("+ idi + ",'" + txtTitle.getText() + "','" + txtPDate.getText() + "'," + idp +")");
				int afr2 = m.execUpdate("INSERT INTO BOOK (DOCID, ISBN) "
				          +"VALUES ("+ idi + ",'" + txtISBN.getText() + "')" );
				
				JOptionPane.showMessageDialog(null, "A new book inserted to document table and book table.");
				
	
				}
				else
				{
					JOptionPane.showMessageDialog(null, "This Book already exists, so new copy is added! ");
				}
				
				
				
			    ArrayList<ArrayList<Object>> result1 = m.execQuery("SELECT * FROM `COPY` WHERE 'DOCID' = '" +  txtBookID.getText() + "' AND 'BID' = '" + txtLID.getText()  + "';");
			    Integer r = result1.size() + 1;
				int afr3 = m.execUpdate("INSERT INTO COPY (DOCID, COPYNO, BID, POSITION) "
				          +"VALUES ("+ idi + "," + r + "," + idl + ",'"+ txtPos.getText() + "')");
			
				JOptionPane.showMessageDialog(null, "1 book inserted into COPY Table");
		}});
		btnAddBook.setBounds(200, 499, 149, 29);
		getContentPane().add(btnAddBook);
		
}

}
