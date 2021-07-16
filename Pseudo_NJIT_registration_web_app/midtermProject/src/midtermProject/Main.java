package midtermProject;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.sql.*;

@WebServlet("/Main")
public class Main extends HttpServlet implements Servlet {

	private static final long serialVersionUID = 1L;

	public Main() {
		super();
	}

	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws IOException, ServletException {
		res.setContentType("text/html");
		PrintWriter out = res.getWriter();
		String title = "NJIT Registration System";
		int radio = -1;
		radio = Integer.parseInt(req.getParameter("rd"));
		String query = "";
		String semester_select = "";
		String reg_result="";
		

		try {
			Class.forName("com.mysql.jdbc.Driver");

			Connection con = DriverManager
					.getConnection("jdbc:mysql://localhost:3306/NJIT?user=root&password=root1234");

			Statement stmt = con.createStatement();
			
			String SQL = "DROP TABLE courses";
		  	 stmt.execute(SQL);
		    
			
		    stmt.execute("CREATE TABLE IF NOT EXISTS courses( courseId char(30), semester char(30), courseName char(30))");
		    stmt.execute("Insert into courses values('CS670', 'Fall2021', 'Artificial Intelligence')");
		    stmt.execute("Insert into courses values('CS677', 'Fall2021', 'Deep Learning')");
		    stmt.execute("Insert into courses values('CS675', 'Spring2022', 'Machine Learning')");
		    stmt.execute("Insert into courses values('CS680', 'Spring2022', 'Linux Programming')");
		    
			
			switch (radio) {
			case 0: {
				
				semester_select = req.getParameter("semester-select");

				if (semester_select.equals("Fall2021"))
					query = "select * from courses where semester='Fall2021'";
				else if (semester_select.equals("Spring2022"))
					query = "select * from courses where semester='Spring2022'";
				
				
				ResultSet rs = stmt.executeQuery(query);
				out.println("<html>\n" + "<head><title>" + title + "</title></head>\n" + "<body bgcolor= \"#ccf2ff\">\n"
						+ "<table align=\"center\" border=1 width=30% height=30%>");
				out.println("<tr><th>Course ID</th><th>Semester</th><th>Course Name</th><tr>");
				while (rs.next()) {
					String id = rs.getString("courseId");
					String s = rs.getString("semester");
					String c = rs.getString("courseName");
					out.println("<tr><td>" + id + "</td><td>" + s + "</td><td>" + c + "</td></tr>");
				}
				out.println("</table>");
				out.println("</html></body>");

				stmt.close();
				break;
			}

			case 1: {
				String cId = req.getParameter("courseid");
				String sem = req.getParameter("semester");
				
				if(!cId.equals("") && !sem.equals("")) {
					query ="Select * from Courses where semester = \'"+sem +"\' and courseid =\'"+cId+"\'";
				}
				
				
				ResultSet rs = stmt.executeQuery(query);
				
				  if(rs.next()) 
				  { 
					  String c = rs.getString("courseName");
					  reg_result = "You are registered in " + c + " for " + sem + "!";
				 
				  }
				
				  else {
						
					  if(!cId.equals("") && !sem.equals("")) 
						  
						  reg_result= "The course is not offered!";
					}
				 
				
				  out.println("<html>\n" + "<head><title>" + title + "</title></head>\n" +
						  "<body bgcolor= \"#ccf2ff\">\n" + "</br></br></br>" +
						  "<h1 align=\"center\"> <font color='black'>" + reg_result + "</font></h1>\n" + "<br><br><br>"+
						  "</body></html>");  
				stmt.close();
				
				break;
			}
			}

			con.close();
		} catch (Exception ex) {
			System.out.println(ex);
			
			 out.println("<html>\n" + "<head><title>" +
			  title + "</title></head>\n" + "<body bgcolor= \"#ccf2ff\">\n" + "</br></br></br>");

			  if(radio==0) 
			  out.println("<h2 align=\"center\"> <font color='red'>" + "Kindly Select Semester!" + "</h2>"); 
			  
			  else
			  out.println("<h2 align=\"center\"> <font color='red'>" + "Course Id or Semester should not be empty!" + "</h2>");
			  
			  out.println("</body></html>"); 
			  
			 
			//System.exit(0);
		}


	}

}
