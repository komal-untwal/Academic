package edu.njit.cs631citylib;

import java.awt.*;
import java.util.ArrayList;
import java.text.ParseException;


import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableModel;

public class AvgFine extends JDialog{
	
	private final JPanel contentPanel = new JPanel();
	private JTable tableAvFn;
	private String sd,ed;
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
	 * @throws ParseException 
	 */
	public AvgFine(String sd,String ed)
	{
		setTitle("CITY LIBRARY");
		
		Database m = Database.getInstance();
		//Long sdt = sd.getTime();
		//Long edt = ed.getTime();
		
		this.sd = sd;
		this.ed = ed;
		
		String[] columnNames = {"BId","BNAME","AVERAGE Fine(cents)"};
		
		ArrayList<ArrayList<Object>> result1 = new ArrayList<ArrayList<Object>>();
		//ArrayList<Object> row = new ArrayList<Object>();
		
				
		m.execUpdate("DROP VIEW IF EXISTS v_Fine;");
		
		m.execUpdate("CREATE VIEW v_Fine AS SELECT br.RID,br.BID,b.lNAME,br.BDTIME,br.RDTIME,(br.RDTIME-br.BDTIME)*20 as FINE FROM BORROWS Br,BRANCH B where BR.BID=b.bid and br.RDTIME > '"+ sd +"' and br.RDTIME< '"+ ed + "' AND (br.RDTIME-br.BDTIME)>20;");
		
		result1 =m.execQuery("select BID,lNAME,avg(FINE)from V_FINE group by BID;");
		
		
		
		Object[][] array = new Object[result1.size()][];
		
		for (int i=0;i<result1.size();i++)
		{
			ArrayList<Object> row = result1.get(i);
			
			array[i] = row.toArray();
			//row.add(result1.get(i));
			//array[0][i] = String.valueOf(result1.get(i));

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
		scrollPane.setBounds(6, 51, 1308, 480);
		contentPanel.add(scrollPane);
	
		tableAvFn = new JTable();
		scrollPane.setViewportView(tableAvFn);
		tableAvFn.setModel(tm);
		

	}

}
