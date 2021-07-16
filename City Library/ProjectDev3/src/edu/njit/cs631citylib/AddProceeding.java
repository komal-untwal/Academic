package edu.njit.cs631citylib;

import java.awt.*;
import java.awt.event.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.*;


public class AddProceeding extends JDialog{
	
	public static void main(String[] args) {
		try {
			AddDocument dialog = new AddDocument();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
public AddProceeding() {
		
		setTitle("CITY LIBRARY");
        
		
        getContentPane().setBackground(new Color(255,250,250));
		//Connect to Database
		Database m = Database.getInstance();
		m.connect();
	
		setBounds(100, 100, 500, 700);
		getContentPane().setLayout(null);
		
		JLabel lblAddProc = new JLabel("Please Enter Proceedings Details");
		lblAddProc.setBounds(74, 78, 250, 31);
		getContentPane().add(lblAddProc);
		
		JLabel lblProcID = new JLabel("Id:");
		lblProcID.setBounds(38, 149, 90, 16);
		getContentPane().add(lblProcID);
		
		JTextField txtProcID = new JTextField();
		txtProcID.setBounds(200, 149, 130, 26);
		getContentPane().add(txtProcID);
		txtProcID.setColumns(10);
		
		
		JLabel lblTitle = new JLabel("Name:");
		lblTitle.setBounds(38, 199, 90, 16);
		getContentPane().add(lblTitle);
		
		JTextField txtTitle = new JTextField();
		txtTitle.setBounds(200, 199, 130, 26);
		getContentPane().add(txtTitle);
		txtTitle.setColumns(10);
		
		JLabel lblCName = new JLabel("Conference Date:");
		lblCName.setBounds(38, 249, 130, 16);
		getContentPane().add(lblCName);
		
		JTextField txtCName = new JTextField();
		txtCName.setBounds(200, 249, 130, 26);
		getContentPane().add(txtCName);
		txtCName.setColumns(10);
		
		JLabel lblCLoc = new JLabel("Conference Location:");
		lblCLoc.setBounds(38, 299, 150, 16);
		getContentPane().add(lblCLoc);
		
		JTextField txtCLoc = new JTextField();
		txtCLoc.setBounds(200, 299, 130, 26);
		getContentPane().add(txtCLoc);
		txtCLoc.setColumns(10);
		
		
		JLabel lblPDate = new JLabel("Date of Publication:");
		lblPDate.setBounds(38, 349, 130, 16);
		getContentPane().add(lblPDate);
		
		JTextField txtPDate = new JTextField();
		txtPDate.setBounds(200, 349, 130, 26);
		getContentPane().add(txtPDate);
		txtPDate.setColumns(10);
		
		JLabel lblPID = new JLabel("Publisher Id:");
		lblPID.setBounds(38, 399, 90, 16);
		getContentPane().add(lblPID);
		
		JTextField txtPID = new JTextField();
		txtPID.setBounds(200, 399, 130, 26);
		getContentPane().add(txtPID);
		txtPID.setColumns(10);
		
		
		JLabel lblLID = new JLabel("Branch No:");
		lblLID.setBounds(38, 449, 90, 16);
		getContentPane().add(lblLID);
		
		JTextField txtLID = new JTextField();
		txtLID.setBounds(200, 449, 130, 26);
		getContentPane().add(txtLID);
		txtLID.setColumns(10);
		
		JLabel lblPos = new JLabel("Position:");
		lblPos.setBounds(38, 499, 90, 16);
		getContentPane().add(lblPos);
		
		JTextField txtPos = new JTextField();
		txtPos.setBounds(200, 499, 130, 26);
		getContentPane().add(txtPos);
		txtPos.setColumns(10);
		
		
		
		JButton btnAddProc = new JButton("Submit");
		btnAddProc.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (txtProcID.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please type ID");
					return;
				}
				/*
				 * if (txtEID.getText().length() <= 0) { JOptionPane.showMessageDialog(null,
				 * "Please specify Editor ID"); return; }
				 */
				if (txtLID.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please specify Branch");
					return;
				}
				
					
				String id = txtProcID.getText();
				Integer idi = Integer.parseInt(id);
				String lid = txtLID.getText();
				Integer idl = Integer.parseInt(lid);
				String pid = txtPID.getText();
				Integer idp = Integer.parseInt(pid);
				//String eid = txtEID.getText();
				//Integer ide = Integer.parseInt(eid);
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
				
				ArrayList<ArrayList<Object>> result = m.execQuery("SELECT * FROM `DOCUMENT` WHERE DOCID = '" + txtProcID.getText() + "';");
				if(result.size() ==0){
			
				m.execUpdate("INSERT INTO DOCUMENT (DOCID, TITLE, PDATE, PUBLISHERID) "
				          +"VALUES ("+ idi + ",'" + txtTitle.getText() + "','" + txtPDate.getText() + "'," + idp +")");
				m.execUpdate("INSERT INTO PROCEEDINGS (DOCID, CDATE, CLOCATION, CEDITOR) "
				          +"VALUES ("+ idi + ",'" + txtCName.getText() + "','" + txtCLoc.getText() + "'," + "NULL" + ")" );
				JOptionPane.showMessageDialog(null, "new proceeding inserted into database");
				}
				else
				{
					JOptionPane.showMessageDialog(null, "Document already exists!!!");
			}
				
			    ArrayList<ArrayList<Object>> result1 = m.execQuery("SELECT * FROM `COPY` WHERE DOCID = '" + txtProcID.getText() + "';");
			    Integer r = result1.size() + 1;
				m.execUpdate("INSERT INTO COPY (DOCID, COPYNO, BID, POSITION) "
				          +"VALUES ("+ idi + "," + r + "," + idl + ",'"+ txtPos.getText() + "')");
				JOptionPane.showMessageDialog(null, "1 proceeding inserted into COPY Table");
		}});
		btnAddProc.setBounds(200, 549, 149, 29);
		getContentPane().add(btnAddProc);
		
}

}
