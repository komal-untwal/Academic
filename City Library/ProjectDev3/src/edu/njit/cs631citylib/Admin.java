package edu.njit.cs631citylib;

import java.awt.*;
import java.awt.event.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.*;


public class Admin extends JDialog{
	private JButton BtnAddReader;
	private JButton BtnAddDocument;
	private JButton BtnSearchDoc;
	private JButton BtnGenerateReport;
	//private JLabel stausLabel;
	
	/**
	 * Create the frame.
	 */
	public Admin() {
		setTitle("CITY LIBRARY");
			
		getContentPane().setBackground(new Color(255,250,250));
		//Connect to Database
		Database m = Database.getInstance();
		m.connect();
	
		setBounds(100, 100, 542, 286);
		getContentPane().setLayout(null);
		
		
		JLabel lblAdminId = new JLabel("Welcome Admin!");
		lblAdminId.setBounds(16, 6, 120, 16);
		getContentPane().add(lblAdminId);
		
		
		JButton btnAddReader = new JButton("Add New Reader");
		btnAddReader.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				AddReader dialog = new AddReader();
				dialog.setModal(true);
				dialog.setVisible(true);
				}
		});
		btnAddReader.setBounds(38, 64, 200, 29);
		getContentPane().add(btnAddReader);
			
	JButton btnAddDocument = new JButton("Add Document Copy");
    btnAddDocument.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					AddDocument dialog = new AddDocument();
					dialog.setModal(true);
					dialog.setVisible(true);
					}	
    });	
    btnAddDocument.setBounds(300, 64, 200, 29);
	getContentPane().add(btnAddDocument);
    
    JButton btnSearchDoc = new JButton("Check Document Status");
    btnSearchDoc.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					SearchDoc dialog = new SearchDoc();
					dialog.setModal(true);
					dialog.setVisible(true);
					}	
    });	
    btnSearchDoc.setBounds(38, 124, 200, 29);
	getContentPane().add(btnSearchDoc);
    
    JButton btnGenerateReport = new JButton("Generate Reports");
    btnGenerateReport.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					GenerateReport dialog = new GenerateReport();
					dialog.setModal(true);
					dialog.setVisible(true);
					}	
    });	
    btnGenerateReport.setBounds(300, 124, 200, 29);
	getContentPane().add(btnGenerateReport);
		
		
	/*
	 * JSeparator separator = new JSeparator(); separator.setBounds(46, 197, 1, 12);
	 * getContentPane().add(separator);
	 * 
	 * JSeparator separator_1 = new JSeparator(); separator_1.setBounds(6, 197, 536,
	 * -60); getContentPane().add(separator_1);
	 * 
	 * JSeparator separator_2 = new JSeparator(); separator_2.setBounds(0, 207, 542,
	 * 16); getContentPane().add(separator_2);
	 */
		
		
		
	}

}
