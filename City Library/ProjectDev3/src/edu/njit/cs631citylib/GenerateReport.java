package edu.njit.cs631citylib;

import java.awt.*;
import java.awt.event.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.ParseException;
import java.util.ArrayList;

import javax.swing.*;


public class GenerateReport extends JDialog{
	public static void main(String[] args) {
		try {
			GenerateReport dialog = new GenerateReport();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	
public GenerateReport() {
		
		
        setTitle("CITY LIBRARY");
		
        getContentPane().setBackground(new Color(255,250,250));
		//Connect to Database
		Database m = Database.getInstance();
		m.connect();
	
		setBounds(100, 100, 500, 550);
		getContentPane().setLayout(null);
		
		JLabel lblGenerateReport = new JLabel("Generate Reports");
		lblGenerateReport.setBounds(38, 14, 200, 31);
		getContentPane().add(lblGenerateReport);
		
		JButton btnBranchInfo = new JButton("Branch Information");
		btnBranchInfo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				BranchInfo dialog = new BranchInfo();
				dialog.setModal(true);
				dialog.setVisible(true);
				}
		});
		btnBranchInfo.setBounds(38, 64, 300, 29);
		getContentPane().add(btnBranchInfo);
			
	JButton btnTopR = new JButton("Top N Readers of Branch I");
	btnTopR.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					TopNBWin dialog = new TopNBWin();
					dialog.setModal(true);
					dialog.setVisible(true);
					}	
    });	
	btnTopR.setBounds(38, 114, 300, 29);
	getContentPane().add(btnTopR);
    
	JButton btnTopLR = new JButton("Top N Readers of Library");
	btnTopLR.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					TopLibB dialog = new TopLibB();
					dialog.setModal(true);
					dialog.setVisible(true);
					}	
    });	
	btnTopLR.setBounds(38, 314, 300, 29);
	getContentPane().add(btnTopLR);
	
    JButton btnTopLB = new JButton("Top N borrowed books from Library");
    btnTopLB.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					TopLibBooks dialog = new TopLibBooks();
					dialog.setModal(true);
					dialog.setVisible(true);
					}	
    });	
    btnTopLB.setBounds(38, 164, 300, 29);
	getContentPane().add(btnTopLB);
	
	JButton btnTopB = new JButton("Top N borrowed books from Branch I");
    btnTopB.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					TopBooks dialog = new TopBooks();
					dialog.setModal(true);
					dialog.setVisible(true);
					}	
    });	
    btnTopB.setBounds(38,364, 300, 29);
	getContentPane().add(btnTopB);
	
	JButton btnTopP = new JButton("Top 10 popular books");
	btnTopP.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					TopYearBooks dialog = new TopYearBooks();
					dialog.setModal(true);
					dialog.setVisible(true);
					}	
    });	
	btnTopP.setBounds(38, 214, 300, 29);
	getContentPane().add(btnTopP);
	
	JButton btnAvFn = new JButton("Compute Average Fine per user");
	btnAvFn.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
			/*
			 * FineCalUI dialog = null; try { dialog = new FineCalUI(); } catch
			 * (ParseException e1) { // TODO Auto-generated catch block
			 * e1.printStackTrace(); }
			 */
					FineCalUI dialog = new FineCalUI();
					dialog.setModal(true);
					dialog.setVisible(true);
					}	
    });	
	btnAvFn.setBounds(38, 264, 300, 29);
	getContentPane().add(btnAvFn);
		
}

}
