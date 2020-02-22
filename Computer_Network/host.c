#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <netdb.h>

#define PORT_NUM 1050
#define IP_ADDR "192.168.132.1" // IP address of server (*** HARDWIRED ***)

void main(void)
{
        unsigned int         server_s, newsockfd;        // Server socket descriptor
        int option,client_size;
        struct sockaddr_in   server_addr,client_addr;     // Server Internet address
        char                 out_buf[100];    // 100-byte output buffer for data
        char                 in_buf[100];     // 100-byte input buffer for data

        option = 1;		// bind ������ ���ִ� �Լ��� ����Ҷ� �ʿ��� ���� 
        
        server_s = socket(AF_INET, SOCK_STREAM, 0);
        server_addr.sin_family      = AF_INET;            // Address family to use
        server_addr.sin_port        = htons(PORT_NUM);    // Port num to use
        server_addr.sin_addr.s_addr = inet_addr(IP_ADDR); // IP address to use

        setsockopt(server_s, SOL_SOCKET, SO_REUSEADDR, &option, sizeof(option) );
        // bind ������ �߻��� ���¿��� bind ��û�� �ϸ� ���� ������ ���� ��� ���´�
		// ���� ���� �ɼǿ� SO_REUSEADDR�� �������ָ� bind ���� ���� ���� ���� 
        
        if(-1 == bind(server_s, (struct sockaddr*)&server_addr, sizeof(server_addr)))
        {
                printf("error\n"); 
                exit(0);
        }
        // bind�� �����ϸ� error�� ��� �� �� ���α׷��� �����Ѵ�. 

        listen(server_s,5);	
		//�ִ� 5���� Ŭ���̾�Ʈ ���� ���� ���� 
		
        client_size = sizeof(client_addr); 
		// client address�� ũ��. 
		
        newsockfd = accept(server_s,(struct sockaddr *)&client_addr, &client_size);
        printf("Connect!\n");
        // client�� ������ ��ٸ��� ������ �Ǹ� Connect! �� ����ϰ� host�� ���� �κ����� �Ѿ��. 
        
        while(1)	// process
        {
                recv(newsockfd, in_buf, sizeof(in_buf), 0);		
                send(newsockfd, in_buf, (strlen(out_buf) + 1), 0);
                // client ���Լ� ���� �����͸� �ٽ� client���� �����ش�. 
        }
        close(server_s);
}
