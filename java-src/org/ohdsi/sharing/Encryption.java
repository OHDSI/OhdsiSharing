package org.ohdsi.sharing;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.security.Key;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;

import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;
import javax.crypto.CipherOutputStream;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

public class Encryption {
	
	private static int RSA_BITS = 4096;
	
	public static void main(String[] args){
		generateKeyPair("s:/temp/public.key", "s:/temp/private.key");
		encryptFile("s:/temp/data.rds", "s:/temp/data.rds.enc", "s:/temp/public.key");
		decryptFile("s:/temp/data.rds.enc", "s:/temp/data2.rds", "s:/temp/private.key");
	}

	public static void generateKeyPair(String publicKeyFileName, String privateKeyFileName) {
		KeyPair keyPair = null;
		KeyPairGenerator keygen;
		try {
			keygen = KeyPairGenerator.getInstance("RSA");
			keygen.initialize(RSA_BITS);
			keyPair = keygen.generateKeyPair();
		} catch (NoSuchAlgorithmException e) {
			throw new RuntimeException(e);
		}
		saveKey(privateKeyFileName, keyPair.getPrivate());
		saveKey(publicKeyFileName, keyPair.getPublic());
	}

	private static Key loadKey(String filename) {
		Key result = null;
		try {
			FileInputStream binFile = new FileInputStream(filename);
			try {
				ObjectInputStream inp = new ObjectInputStream(binFile);
				try {
					result = (Key) inp.readObject();
				} catch (ClassNotFoundException e) {
					e.printStackTrace();
				} finally {
					inp.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		} catch (FileNotFoundException e) {
			throw new RuntimeException(e);
		}
		return result;
	}

	private static void saveKey(String filename, Key key) {
		try {
			FileOutputStream binFile = new FileOutputStream(filename);
			try {
				ObjectOutputStream out = new ObjectOutputStream(binFile);
				out.writeObject(key);
				out.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		} catch (FileNotFoundException e) {
			throw new RuntimeException(e);
		}
	}

	public static void encryptFile(String sourceFileName, String targetFileName, String publicKeyFileName) {
		try {
			Key publicKey = loadKey(publicKeyFileName);

			// Generate random symmetric key (AES algorithm):
			KeyGenerator kgen = KeyGenerator.getInstance("AES");
			kgen.init(128);
			SecretKey aesKey = kgen.generateKey();

			// Create encoding cipher using public key (RSA algorithm):
			Cipher rsaCipher = Cipher.getInstance("RSA/ECB/PKCS1Padding");
			rsaCipher.init(Cipher.ENCRYPT_MODE, publicKey);

			// Open file output stream:
			FileOutputStream file = new FileOutputStream(targetFileName);

			// Encode symmetric key using encoding cipher, and write to file:
			file.write(rsaCipher.doFinal(aesKey.getEncoded()));

			// Open encrypted stream using symmetric key (AES algorithm):
			Cipher cipher = Cipher.getInstance("AES");
			cipher.init(Cipher.ENCRYPT_MODE, aesKey);
			CipherOutputStream out = new CipherOutputStream(file, cipher);
			BufferedWriter bufferedWrite = new BufferedWriter(new OutputStreamWriter(out));

			// Open input stream and copy data:
			FileInputStream in = new FileInputStream(sourceFileName);
			copyStream(in, out);

			// Close streams:
			in.close();
			bufferedWrite.close();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static void decryptFile(String sourceFileName, String targetFileName, String privateKeyFileName){
		try {
			Key privateKey = loadKey(privateKeyFileName);

			// Generate cipher using private key (RSA algorithm):
			Cipher rsaCipher = Cipher.getInstance("RSA/ECB/PKCS1Padding");
			rsaCipher.init(Cipher.DECRYPT_MODE, privateKey);

			// Open file:
			FileInputStream textFileStream = new FileInputStream(sourceFileName);

			// Read encrypted symmetric key, and decrypt using private key:
			byte[] encKey = new byte[RSA_BITS/8];
			textFileStream.read(encKey);
			Key aesKey = new SecretKeySpec(rsaCipher.doFinal(encKey), "AES");

			// Create decryption stream (AES algorithm):
			Cipher aesCipher = Cipher.getInstance("AES");
			aesCipher.init(Cipher.DECRYPT_MODE, aesKey);
			CipherInputStream in = new CipherInputStream(textFileStream, aesCipher);

			// Decrypt stream:
			FileOutputStream out = new FileOutputStream(targetFileName);
			copyStream(in, out);

			out.close();
			in.close();
		} catch (Exception e){
			throw new RuntimeException(e);
		}
	}

	private static void copyStream(InputStream in, OutputStream out) {
		int bufferSize = 1024;
		int bytes;
		byte[] buffer;
		buffer = new byte[bufferSize];
		try {
			while ((bytes = in.read(buffer)) != -1) {
				if (bytes == 0) {
					bytes = in.read();
					if (bytes < 0)
						break;
					out.write(bytes);
					out.flush();
					continue;
				}
				out.write(buffer, 0, bytes);
				out.flush();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
