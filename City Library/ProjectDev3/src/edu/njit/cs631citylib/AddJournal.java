package edu.njit.cs631citylib;

import java.awt.*;
import java.awt.event.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.*;


public class AddJournal extends JDialog{
	
	public static void main(String[] args) {
		try {
			AddDocument dialog = new AddDocument();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
public AddJournal() {
		
		setTitle("CITY LIBRARY");
        
		
        getContentPane().setBackground(new Color(255,250,250));
		//Connect to Database
		Database m = Database.getInstance();
		m.connect();
	
		setBounds(100, 100, 500, 800);
		getContentPane().setLayout(null);
		
		JLabel lblAddJounal = new JLabel("Please Enter Journal Details");
		lblAddJounal.setBounds(74, 78, 200, 31);
		getContentPane().add(lblAddJounal);
		
		JLabel lblJournalID = new JLabel("Journal Id:");
		lblJournalID.setBounds(38, 149, 90, 16);
		getContentPane().add(lblJournalID);
		
		JTextField txtJournalID = new JTextField();
		txtJournalID.setBounds(200, 149, 130, 26);
		getContentPane().add(txtJournalID);
		txtJournalID.setColumns(10);
		
		
		JLabel lblTitle = new JLabel("Name:");
		lblTitle.setBounds(38, 199, 90, 16);
		getContentPane().add(lblTitle);
		
		JTextField txtTitle = new JTextField();
		txtTitle.setBounds(200, 199, 130, 26);
		getContentPane().add(txtTitle);
		txtTitle.setColumns(10);
		
		JLabel lblVolume = new JLabel("Volume:");
		lblVolume.setBounds(38, 249, 90, 16);
		getContentPane().add(lblVolume);
		
		JTextField txtVolume = new JTextField();
		txtVolume.setBounds(200, 249, 130, 26);
		getContentPane().add(txtVolume);
		txtVolume.setColumns(10);
		
		JLabel lblIssue = new JLabel("Issue No:");
		lblIssue.setBounds(38, 299, 90, 16);
		getContentPane().add(lblIssue);
		
		JTextField txtIssue = new JTextField();
		txtIssue.setBounds(200, 299, 130, 26);
		getContentPane().add(txtIssue);
		txtIssue.setColumns(10);
		
		JLabel lblScope = new JLabel("Scope:");
		lblScope.setBounds(38, 349, 90, 16);
		getContentPane().add(lblScope);
		
		JTextField txtScope = new JTextField();
		txtScope.setBounds(200, 349, 130, 26);
		getContentPane().add(txtScope);
		txtScope.setColumns(10);
		
		JLabel lblPDate = new JLabel("Date of Publication:");
		lblPDate.setBounds(38, 399, 130, 16);
		getContentPane().add(lblPDate);
		
		JTextField txtPDate = new JTextField();
		txtPDate.setBounds(200, 399, 130, 26);
		getContentPane().add(txtPDate);
		txtPDate.setColumns(10);
		
		JLabel lblPID = new JLabel("Publisher Id:");
		lblPID.setBounds(38, 449, 90, 16);
		getContentPane().add(lblPID);
		
		JTextField txtPID = new JTextField();
		txtPID.setBounds(200, 449, 130, 26);
		getContentPane().add(txtPID);
		txtPID.setColumns(10);
		
		JLabel lblEID = new JLabel("Editor Id:");
		lblEID.setBounds(38, 499, 90, 16);
		getContentPane().add(lblEID);
		
		JTextField txtEID = new JTextField();
		txtEID.setBounds(200, 499, 130, 26);
		getContentPane().add(txtEID);
		txtEID.setColumns(10);
		
		JLabel lblLID = new JLabel("Branch No:");
		lblLID.setBounds(38, 549, 90, 16);
		getContentPane().add(lblLID);
		
		JTextField txtLID = new JTextField();
		txtLID.setBounds(200, 549, 130, 26);
		getContentPane().add(txtLID);
		txtLID.setColumns(10);
		
		JLabel lblPos = new JLabel("Position:");
		lblPos.setBounds(38, 599, 90, 16);
		getContentPane().add(lblPos);
		
		JTextField txtPos = new JTextField();
		txtPos.setBounds(200, 599, 130, 26);
		getContentPane().add(txtPos);
		txtPos.setColumns(10);
		
		
		
		JButton btnAddJournal = new JButton("Submit");
		btnAddJournal.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (txtJournalID.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please type ID");
					return;
				}
				if (txtIssue.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please specify Issue NO");
					return;
				}
				if (txtLID.getText().length() <= 0) {
					JOptionPane.showMessageDialog(null, "Please specify Branch");
					return;
				}
				
					
				String id = txtJournalID.getText();
				Integer idi = Integer.parseInt(id);
				String lid = txtLID.getText();
				Integer idl = Integer.parseInt(lid);
				String pid = txtPID.getText();
				Integer idp = Integer.parseInt(pid);
				String eid = txtEID.getText();
				Integer ide = Integer.parseInt(eid);
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
				//NO chief editor table in our database
				ArrayList<ArrayList<Object>> resulte = m.execQuery("SELECT * FROM `PERSON` WHERE PID = '" + txtEID.getText() + "';");
				if(resultp.size()==0){
					
						JOptionPane.showMessageDialog(null, "No EDITOR  WITH THIS ID. CANNOT INSERT");
					
					
				}
				ArrayList<ArrayList<Object>> resultDoc = m.execQuery("SELECT * FROM `DOCUMENT` WHERE DOCID = '" + txtJournalID.getText() + "';");
				ArrayList<ArrayList<Object>> resultVol = m.execQuery("SELECT * FROM `JOURNAL_VOLUME` WHERE VOLUME_NO = '" + txtVolume.getText() + "';");
				ArrayList<ArrayList<Object>> resultIss = m.execQuery("SELECT * FROM `JOURNAL_ISSUE` WHERE ISSUE_NO = '" + txtIssue.getText() + "';");
				
				if(resultDoc.size() ==0 && resultVol.size() ==0 && resultIss.size()==0){
			
				m.execUpdate("INSERT INTO DOCUMENT (DOCID, TITLE, PDATE, PUBLISHERID) "
				          +"VALUES ("+ idi + ",'" + txtTitle.getText() + "','" + txtPDate.getText() + "'," + idp +")");
				m.execUpdate("INSERT INTO JOURNAL_VOLUME (DOCID, VOLUME_NO, EDITOR) "
				          +"VALUES ("+ idi + ",'" + txtVolume.getText() + "'," + ide + ")" );
				m.execUpdate("INSERT INTO JOURNAL_ISSUE (DOCID, ISSUE_NO, SCOPE) "
				          +"VALUES ("+ idi + ",'" + txtIssue.getText() + "','" + txtScope.getText() + "')" );
				
				JOptionPane.showMessageDialog(null, "New journal issue inserted.");
				}
				if(resultDoc.size() !=0)
				{
					JOptionPane.showMessageDialog(null, "Document already exists.. new Copy added!!!!!");
				}
				if(resultVol.size() !=0)
				{
					JOptionPane.showMessageDialog(null, "Journal volume already exists.. new Copy added!!!!!");
				}
				if(resultIss.size() !=0)
				{
					JOptionPane.showMessageDialog(null, "Journal Issue already exists.. new Copy added!!!!!");
				}
				
				
			    ArrayList<ArrayList<Object>> result1 = m.execQuery("SELECT * FROM `COPY` WHERE DOCID = '" + txtJournalID.getText() + "';");
			    Integer r = result1.size() + 1;
				m.execUpdate("INSERT INTO COPY (DOCID, COPYNO, BID, POSITION) "
				          +"VALUES ("+ idi + "," + r + "," + idl + ",'"+ txtPos.getText() + "')");
				JOptionPane.showMessageDialog(null, "1 copy of journal inserted into COPY Table");
			
		}});
		btnAddJournal.setBounds(200, 649, 149, 29);
		getContentPane().add(btnAddJournal);
		
}


}

