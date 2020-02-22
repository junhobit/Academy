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

        option = 1;		// bind 에러를 없애는 함수를 사용할때 필요한 변수 
        
        server_s = socket(AF_INET, SOCK_STREAM, 0);
        server_addr.sin_family      = AF_INET;            // Address family to use
        server_addr.sin_port        = htons(PORT_NUM);    // Port num to use
        server_addr.sin_addr.s_addr = inet_addr(IP_ADDR); // IP address to use

        setsockopt(server_s, SOL_SOCKET, SO_REUSEADDR, &option, sizeof(option) );
        // bind 에러가 발생한 상태에서 bind 요청을 하면 이전 소켓은 오래 살아 남는다
		// 따라서 소켓 옵션에 SO_REUSEADDR을 지정해주면 bind 에러 없이 실행 가능 
        
        if(-1 == bind(server_s, (struct sockaddr*)&server_addr, sizeof(server_addr)))
        {
                printf("error\n"); 
                exit(0);
        }
        // bind를 실패하면 error를 출력 한 후 프로그램을 종료한다. 

        listen(server_s,5);	
		//최대 5개의 클라이언트 까지 접근 가능 
		
        client_size = sizeof(client_addr); 
		// client address의 크기. 
		
        newsockfd = accept(server_s,(struct sockaddr *)&client_addr, &client_size);
        printf("Connect!\n");
        // client가 들어오길 기다리다 연결이 되면 Connect! 를 출력하고 host의 수행 부분으로 넘어간다. 
        
        while(1)	// process
        {
                recv(newsockfd, in_buf, sizeof(in_buf), 0);		
                send(newsockfd, in_buf, (strlen(out_buf) + 1), 0);
                // client 에게서 받은 데이터를 다시 client에게 보내준다. 
        }
        close(server_s);
}
