package edu.njit.cs631citylib;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.WindowEvent;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.JButton;
import javax.swing.JLabel;

public class Reserve extends JDialog {

	private final JPanel contentPanel = new JPanel();
	private JTable tableDocReserveResult;
	
	public static void main(String[] args) {
		try {
			Database m = Database.getInstance();
			m.connect();
			Reserve dialog = new Reserve("1");
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	/**
	 * Create the dialog.
	 */
	public Reserve(String cardNumber) {
		setTitle("City Library");
		
		Database m = Database.getInstance();
		
		String[] columnNames = {"ReserveNo", "ReaderId", "DocId", "CopyNo", "LibId", "DayTime"};
		ArrayList<ArrayList<Object>> reserveResult = new ArrayList<ArrayList<Object>>();
		
		reserveResult = m.execQuery("SELECT `RESERVATION_NO`, `RID`, `DOCID`, `COPYNO`, `BID`, `DTIME` FROM `RESERVES` WHERE `RID` = " + cardNumber + ";");

		//get current time
		long b2 = System.currentTimeMillis();
		
		//get today's 6PM
		Calendar cl = Calendar.getInstance(TimeZone.getDefault());
		int year = cl.get(Calendar.YEAR);
		int month = cl.get(Calendar.MONTH);
		int day = cl.get(Calendar.DAY_OF_MONTH);
		cl.set(year, month, day, 18, 0, 0);
		
		//Calculate yesterday 6PM 
		long y = cl.getTimeInMillis() - 86400000;

			
		Object[][] array = new Object[reserveResult.size()][];
		int j = 0;
		for (int i = 0; i < reserveResult.size(); i++) {
			ArrayList<Object> row = reserveResult.get(i);
			array[j] = row.toArray();
			j = j+1; 
			
			//get reserve time
			/*
			 * Date r = (Date)row.get(5);
			 * 
			 * //Check if the reserve time is less than yesterday 6PM if (r.getTime()-y >
			 * 0){ //Check if current time is before 6PM if (b2 - cl.getTimeInMillis()<0){
			 * array[j] = row.toArray(); j = j+1; } //If current time is after 6PM if (b2 -
			 * cl.getTimeInMillis()>=0){ //If reserve time is after 6PM if (r.getTime() -
			 * cl.getTimeInMillis()>0 ){ array[j] = row.toArray(); j = j+1; } } }
			 */
		}
		
	
		if (reserveResult == null || reserveResult.size() <= 0||array[0] == null){
			JOptionPane.showMessageDialog(null, "No Reserved Documents");
		}

		DefaultTableModel tm = new DefaultTableModel(array, columnNames);
		
		setBounds(100, 100, 1053, 610);
		getContentPane().setLayout(null);
		contentPanel.setBounds(0, 0, 1047, 582);
		contentPanel.setBackground(new Color(255,250,250));
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel);
		contentPanel.setLayout(null);
	
		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(42, 72, 966, 391);
		contentPanel.add(scrollPane);
	    scrollPane.setBackground(new Color(255,250,250));
		tableDocReserveResult = new JTable();
		scrollPane.setViewportView(tableDocReserveResult);
		tableDocReserveResult.setModel(tm);
		
		JLabel lblNewLabel = new JLabel("Reserved Docs:");
		lblNewLabel.setBounds(43, 24, 200, 36);
		contentPanel.add(lblNewLabel);
		
		
	
	}
}
