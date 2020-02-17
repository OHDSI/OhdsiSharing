package org.ohdsi.sharing;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Vector;

import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.ChannelSftp.LsEntry;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

public class Sftp {

	private ChannelSftp channel;
	private Session session;

	public static void main(String[] args) throws Exception {
//		Sftp sftp = new Sftp("c:/temp/sftp/study-data-site-test", "study-data-site-test");
		Sftp sftp = new Sftp("c:/temp/sftp/study-coordinator-test", "study-coordinator-test");
		System.out.println("Connected");

//		sftp.putFile("c:/temp/sftp/cars.csv", "cars5.csv");
//		System.out.println("File Uploaded to FTP Server Sucessfully.");

		try {
//			sftp.rm("cars5.csv");
			System.out.println(sftp.pwd());
			String[] files = sftp.ls("./");	
			for (int i = 0; i < files.length / 2; i++) {
				System.out.println(files[i] + "\t" + files[i + files.length / 2]);
			}
		} finally {
			sftp.disconnect();
			System.out.println("Disconnected");
		}
	}

	public Sftp(String privateKeyFileName, String userName) throws JSchException {
		JSch jsch = new JSch();
		jsch.addIdentity(privateKeyFileName);
		jsch.setKnownHosts(Sftp.class.getResourceAsStream("ohdsi-known-hosts"));
		session = jsch.getSession(userName, "sftp.ohdsi.org");
		java.util.Properties config = new java.util.Properties();
		config.put("StrictHostKeyChecking", "no");
		session.setConfig(config);
		session.connect();
		channel = (ChannelSftp) session.openChannel("sftp");
		channel.connect();
	}

	public void putFile(String localFileName, String remoteFilename) throws FileNotFoundException {
		File f = new File(localFileName);
		try {
			channel.put(new FileInputStream(f), remoteFilename);
		} catch (SftpException e) {
			throw new RuntimeException(
					"Unable to write file to " + remoteFilename + ". Reason: " + e.getLocalizedMessage());
		}
	}

	public void cd(String remoteFolder) throws SftpException {
		channel.cd(remoteFolder);
	}

	public void mkdir(String remoteFolder) throws SftpException {
		channel.mkdir(remoteFolder);
	}

	public void rmdir(String remoteFolder) throws SftpException {
		channel.rmdir(remoteFolder);
	}

	public String pwd() throws SftpException {
		return channel.pwd();
	}

	public void rm(String remoteFile) throws SftpException {
		channel.rm(remoteFile);
	}

	@SuppressWarnings("unchecked")
	public String[] ls(String remoteFolder) throws SftpException {
		Vector<LsEntry> fileVector = channel.ls(remoteFolder);
		// Don't know how to process complex Java objects in R, so returning a string array, where first 
		// half is file names, second half is file types.
		String[] result = new String[fileVector.size() * 2];
		int offset = fileVector.size();
		for (int i = 0; i < fileVector.size(); i++) {
			LsEntry entry = (LsEntry) fileVector.get(i);
			if (entry.getAttrs().isDir()) 
				result[offset + i] = "DIR";
			else if (entry.getAttrs().isLink())
				result[offset + i] = "LINK";
			else 
				result[offset + i] = "FILE";
			result[i] = entry.getFilename();
		}
		return result;
	}

	public void rename(String oldRemoteFilename, String newRemoteFilename) throws SftpException {
		channel.rename(oldRemoteFilename, newRemoteFilename);
	}

	public void getFile(String remoteFilename, String localFileName) throws SftpException {
		channel.get(remoteFilename, localFileName);
	}

	public void disconnect() {
		session.disconnect();
	}
}
