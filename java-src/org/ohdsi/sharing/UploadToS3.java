package org.ohdsi.sharing;

import java.io.File;
import java.io.IOException;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.profile.ProfileCredentialsProvider;
import com.amazonaws.auth.EnvironmentVariableCredentialsProvider;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.PutObjectRequest;

import java.util.UUID;

public class UploadToS3 {

	public static void uploadFile(String uploadFileName, String bucketName, String keyName) {		
	    AmazonS3 s3client = new AmazonS3Client(new EnvironmentVariableCredentialsProvider());
        try {
            
            keyName = keyName = keyName + "_" + UUID.randomUUID(); 
            
            File file = new File(uploadFileName);
            s3client.putObject(new PutObjectRequest(
            		                 bucketName, keyName, file));

         } catch (AmazonServiceException ase) {
            System.out.println("Caught an AmazonServiceException, which " +
            		"means your request made it " +
                    "to Amazon S3, but was rejected with an error response" +
                    " for some reason.");
            System.out.println("Error Message:    " + ase.getMessage());
            System.out.println("HTTP Status Code: " + ase.getStatusCode());
            System.out.println("AWS Error Code:   " + ase.getErrorCode());
            System.out.println("Error Type:       " + ase.getErrorType());
            System.out.println("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
            System.out.println("Caught an AmazonClientException, which " +
            		"means the client encountered " +
                    "an internal error while trying to " +
                    "communicate with S3, " +
                    "such as not being able to access the network.");
            System.out.println("Error Message: " + ace.getMessage());
        }
    }	  
}


// import java.io.File;
// 
// import com.amazonaws.AmazonClientException;
// import com.amazonaws.auth.profile.ProfileCredentialsProvider;
// import com.amazonaws.services.s3.transfer.TransferManager;
// import com.amazonaws.services.s3.transfer.Upload;
// 
// public class UploadObjectMultipartUploadUsingHighLevelAPI {
// 
//     public static void main(String[] args) throws Exception {
//         String existingBucketName = "*** Provide existing bucket name ***";
//         String keyName            = "*** Provide object key ***";
//         String filePath           = "*** Path to and name of the file to upload ***";  
//         
//         TransferManager tm = new TransferManager(new ProfileCredentialsProvider());        
//         System.out.println("Hello");
//         // TransferManager processes all transfers asynchronously, 
//         // so this call will return immediately.
//         Upload upload = tm.upload(
//         		existingBucketName, keyName, new File(filePath));
//         System.out.println("Hello2");
// 
//         try {
//         	// Or you can block and wait for the upload to finish
//         	upload.waitForCompletion();
//         	System.out.println("Upload complete.");
//         } catch (AmazonClientException amazonClientException) {
//         	System.out.println("Unable to upload file, upload was aborted.");
//         	amazonClientException.printStackTrace();
//         }
//     }
// }