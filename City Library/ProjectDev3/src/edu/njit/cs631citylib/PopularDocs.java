package edu.njit.cs631citylib;

import java.awt.*;
import java.awt.event.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableModel;

public class PopularDocs extends JDialog{
	
	private final JPanel contentPanel = new JPanel();
	private JTable tableTopP;
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			Database m = Database.getInstance();
			m.connect();
			Borrow dialog = new Borrow("1");
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public PopularDocs(String y) {
		setTitle("CITY LIBRARY");
		
		Database m = Database.getInstance();
		Integer year = Integer.parseInt(y);
		
		
		String[] columnNames = {"DOCID","No. of Borrows"};
		ArrayList<ArrayList<Object>> result = new ArrayList<ArrayList<Object>>();
		
		result = m.execQuery("SELECT `DOCID`, COUNT(*) FROM `BORROWS` WHERE YEAR(BDTIME) = '" + year + "' GROUP BY DOCID ORDER BY COUNT(*) DESC LIMIT 10;");

		Object[][] array = new Object[result.size()][];
		for (int i = 0; i < result.size(); i++) {
			ArrayList<Object> row = result.get(i);
			
			array[i] = row.toArray();
		}

		

		DefaultTableModel tm = new DefaultTableModel(array, columnNames);
		
		setBounds(100, 100, 1358, 610);
		getContentPane().setLayout(null);
		contentPanel.setBounds(0, 0, 1352, 582);
		contentPanel.setBackground(new Color(255,250,250));
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel);
		contentPanel.setLayout(null);
	
		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(26, 51, 1308, 480);
		contentPanel.add(scrollPane);
	
		tableTopP = new JTable();
		scrollPane.setViewportView(tableTopP);
		tableTopP.setModel(tm);

	}




}
