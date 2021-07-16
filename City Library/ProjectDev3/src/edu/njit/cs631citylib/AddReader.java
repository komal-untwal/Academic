package edu.njit.cs631citylib;

import java.awt.*;
import java.awt.event.*;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.*;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import com.mysql.jdbc.*;

import javax.swing.JList;
import javax.swing.JSpinner;

public class AddReader extends JDialog{
	
	
	public static void main(String[] args) {
		try {
			AddReader dialog = new AddReader();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public AddReader() {
		
		setTitle("CITY LIBRARY");
        
		
        getContentPane().setBackground(new Color(255,250,250));
		//Connect to Database
		Database m = Database.getInstance();
		m.connect();
	
		setBounds(100, 100, 500, 550);
		getContentPane().setLayout(null);
		
		JLabel lblAddReader = new JLabel("Please Enter Reader Details!");
		lblAddReader.setBounds(74, 78, 250, 31);
		getContentPane().add(lblAddReader);
		
		JLabel lblReaderID = new JLabel("Reader ID: ");
		lblReaderID.setBounds(38, 149, 90, 16);
		getContentPane().add(lblReaderID);
		
		JTextField txtReaderID = new JTextField();
		txtReaderID.setBounds(125, 149, 130, 26);
		getContentPane().add(txtReaderID);
		txtReaderID.setColumns(10);
		
		JLabel lblReaderType = new JLabel("Reader Type:");
		lblReaderType.setBounds(38, 199, 90, 16);
		getContentPane().add(lblReaderType);
		
		JTextField txtReaderType = new JTextField();
		txtReaderType.setBounds(125, 199, 130, 26);
		getContentPane().add(txtReaderType);
		txtReaderType.setColumns(10);
		
		JLabel lblReaderName = new JLabel("Name:");
		lblReaderName.setBounds(38, 249, 90, 16);
		getContentPane().add(lblReaderName);
		
		JTextField txtReaderName = new JTextField();
		txtReaderName.setBounds(125, 249, 130, 26);
		getContentPane().add(txtReaderName);
		txtReaderName.setColumns(10);
		
		JLabel lblReaderAdd = new JLabel("Address:");
		lblReaderAdd.setBounds(38, 299, 90, 16);
		getContentPane().add(lblReaderAdd);
		
		JTextField txtReaderAdd = new JTextField();
		txtReaderAdd.setBounds(125, 299, 130, 26);
		getContentPane().add(txtReaderAdd);
		txtReaderAdd.setColumns(20);
		
		JLabel lblReaderPhn = new JLabel("Phone No:");
		lblReaderPhn.setBounds(38, 349, 90, 16);
		getContentPane().add(lblReaderPhn);
		
		JTextField txtReaderPhn = new JTextField();
		txtReaderPhn.setBounds(125, 349, 130, 26);
		getContentPane().add(txtReaderPhn);
		txtReaderPhn.setColumns(20);
		
		JButton btnAddReader = new JButton("Submit");
		btnAddReader.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (txtReaderID.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please type ID");
					return;
				}
				if (txtReaderType.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please specify type");
					return;
				}
				if (txtReaderName.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please specify name");
					return;
				}
				if (txtReaderAdd.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please specify address");
					return;
				}
				if (txtReaderPhn.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please specify Phone Number");
					return;
				}
				
				String id = txtReaderID.getText();
				Integer idi = Integer.parseInt(id);
				String ty = txtReaderType.getText();
				String nm = txtReaderName.getText();
				String ad = txtReaderAdd.getText();
				String ph = txtReaderPhn.getText();
				
				ArrayList<ArrayList<Object>> resultl = m.execQuery("SELECT * FROM `READER` WHERE RID = '" + txtReaderID.getText() + "';");
				if(resultl.size()!=0){
					
						JOptionPane.showMessageDialog(null, "Reader already exists. CANNOT INSERT");
					
					
				}
				//add phone number
				int afr = m.execUpdate("INSERT INTO READER (RID, RTYPE, RNAME, RADDRESS, PHONE_NO) "
				          +"VALUES ("+ idi + ",'" + txtReaderType.getText() + "','" + txtReaderName.getText() + "','" + txtReaderAdd.getText() + "','" + txtReaderPhn.getText() +",')");
				if(afr>0){
					JOptionPane.showMessageDialog(null, "1 reader inserted to database");
				}
				/*ArrayList<ArrayList<Object>> result = m.execQuery("SELECT 'loginID', 'password' FROM `ADMIN` WHERE loginID = '" + txtAdminID.getText() + "' AND password = '" + txtAdminPwd.getText() + "';");
				if (result == null || result.size() != 1) {
					System.out.println(result.size());
					JOptionPane.showMessageDialog(null, "Invalid ID or password");
				} else {
					Admin dialog = new Admin();
					dialog.setModal(true);
					dialog.setVisible(true);
				}*/
			}
			
		});
		btnAddReader.setBounds(125, 399, 149, 29);
		getContentPane().add(btnAddReader);
	}

}
