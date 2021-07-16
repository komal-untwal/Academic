package edu.njit.cs631citylib;
import java.awt.*;
import java.awt.event.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.*;

public class AddDocument extends JDialog{
	
	public static void main(String[] args) {
		try {
			AddDocument dialog = new AddDocument();
			dialog.dispose();
			//dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
public AddDocument() {
		
		setTitle("CITY LIBRARY");
        
		
        getContentPane().setBackground(new Color(255,250,250));
		//Connect to Database
		Database m = Database.getInstance();
		m.connect();
	
		setBounds(100, 100, 400, 300);
		getContentPane().setLayout(null);
		
		JLabel lblAddReader = new JLabel("Add New Document");
		lblAddReader.setBounds(38, 14, 200, 31);
		getContentPane().add(lblAddReader);
		
		JButton btnAddBook = new JButton("Add New Book");
		btnAddBook.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				AddBook dialog = new AddBook();
				dialog.setModal(true);
				dialog.setVisible(true);
				}
		});
		btnAddBook.setBounds(38, 64, 200, 29);
		getContentPane().add(btnAddBook);
			
	JButton btnAddJournal = new JButton("Add New Journal");
	btnAddJournal.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					AddJournal dialog = new AddJournal();
					dialog.setModal(true);
					dialog.setVisible(true);
					}	
    });	
	btnAddJournal.setBounds(38, 114, 200, 29);
	getContentPane().add(btnAddJournal);
    
    JButton btnAddProc = new JButton("Add New Proceedings");
    btnAddProc.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					AddProceeding dialog = new AddProceeding();
					dialog.setModal(true);
					dialog.setVisible(true);
					}	
    });	
    btnAddProc.setBounds(38, 164, 200, 29);
	getContentPane().add(btnAddProc);
		
}


}

