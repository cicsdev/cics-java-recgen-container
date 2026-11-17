/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* SAMPLE                                                                 */
/*                                                                        */
/* (c) Copyright IBM Corp. 2016 All Rights Reserved                       */       
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or disclosure */
/* restricted by GSA ADP Schedule Contract with IBM Corp                  */
/*                                                                        */ 

package com.ibm.cicsdev.jzostest;

import java.text.MessageFormat;
import com.ibm.cics.server.Channel;
import com.ibm.cics.server.Container;
import com.ibm.cics.server.InvalidProgramIdException;
import com.ibm.cics.server.InvalidRequestException;
import com.ibm.cics.server.Program;
import com.ibm.cics.server.Task;
import com.ibm.cicsdev.bean.JZOSCommareaWrapper;

public class JZOSprog {

	public static final String PROGLINK = "EDUPGM";  // Linked to COBOL program
	public static final String CHANNEL = "EDUCHANNEL";  // Channel passed to COBOL program
	public static final String CONTAINER = "EDUCONTAINER";  // Container used by COBOL program

	public static void main(String[] args)          
	{

		// Get details about our current CICS task
		Task task = Task.getTask();
        task.out.println(" - Starting JOZSprog");
		// Wrapper objects for input and output container data
		JZOSCommareaWrapper cwIn = null;
		JZOSCommareaWrapper cwOut = null;

		// Set the input data fields
		Short binarydigit = 1;
		String charstring = "hello";
		Short numericstring = 1234;
		Integer packeddigit = 123456789;
		Integer signedpackeddigit = -100;
		String bool = "1";

		cwIn = new JZOSCommareaWrapper();
		cwIn.setBinaryDigit(binarydigit );
		cwIn.setCharacterString(charstring );
		cwIn.setNumericString(numericstring );
		cwIn.setPackedDigit(packeddigit );
		cwIn.setSignedPacked(signedpackeddigit );
		cwIn.setBool(bool );

		// Create a reference to the CICS program
		Program prog = new Program();
		prog.setName(PROGLINK);	

		// Create byte array for input container from wrapper
		byte[] inputData = cwIn.getByteBuffer();

		try {
			// Create the channel and put the input data in the container
			Channel channel = task.createChannel(CHANNEL);
			Container container = channel.createContainer(CONTAINER);
			container.put(inputData);

			// Link to target CICS program, passing the channel
			prog.link(channel);

			// Build output record from the updated container
			cwOut = new JZOSCommareaWrapper(container.get());

		// Catch specific JCICS exception
		} catch (InvalidProgramIdException e) { 
			String msg = "ERROR: Invalid CICS Program {0} with msg({1})";
			task.out.println(MessageFormat.format(msg, PROGLINK, e.getMessage()));

		// Catch any other exception and force a rollback of CICS UOW
		} catch (Exception e) {
			String msg = "ERROR: Exception on link to {0} with msg({1})";
			task.out.println(MessageFormat.format(msg, PROGLINK, e.getMessage()));
			// Rollback the CICS Task 
			try 
			{
				task.rollback();
			} catch (InvalidRequestException e1) {		
				// If this fails, then throw Runtime Exception
				throw new RuntimeException(e1);
			}
		} 

		String msg = "JZOSProg: Returned from {0} with rc({1}) {2}";
		task.out.println(MessageFormat.format(msg, PROGLINK,cwOut.getResultCode(), cwOut.getResultText()));

	}

}
