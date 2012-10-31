package LUDOSimulator;

public class AnIPlay implements LUDOPlayer {

    LUDOBoard board;

    public AnIPlay(LUDOBoard board){
        this.board = board;

    }
   
    public static void main(String[] args) {

        LUDO y = new LUDO();
        y.play();

            }


   
    public void play() {
        board.print("AniPlayer playing");
         long time = System.currentTimeMillis();
         int[] result = new int[4];

         board.setPlayer(new AnIPlay(board),LUDOBoard.RED);
         board.setPlayer(new RandomLUDOPlayer(board),LUDOBoard.YELLOW);
         board.setPlayer(new PacifisticLUDOPlayer(board),LUDOBoard.BLUE);
         board.setPlayer(new SemiSmartLUDOPlayer(board),LUDOBoard.GREEN);
         try {
         for(int i=0;i<1000;i++) {
         board.play();
         board.kill();
         result[0]+=board.getPoints()[0];
         result[1]+=board.getPoints()[1];
         result[2]+=board.getPoints()[2];
         result[3]+=board.getPoints()[3];
         board.reset();
         board.setPlayer(new AnIPlay (board),LUDOBoard.RED);
         board.setPlayer(new RandomLUDOPlayer(board),LUDOBoard.YELLOW);
         board.setPlayer(new PacifisticLUDOPlayer(board),LUDOBoard.BLUE);
         board.setPlayer(new SemiSmartLUDOPlayer(board),LUDOBoard.GREEN);
         if((i%500)==0) System.out.print(".");
         }
         } catch (InterruptedException e) {
         e.printStackTrace();
         }
         System.out.println();
         System.out.println("Simulation took "+(System.currentTimeMillis()-time)+" miliseconds");
         System.out.println("RESULT:");
         System.out.println("YELLOW Player: "+result[0]);
         System.out.println("RED    Player: "+result[1]);
         System.out.println("BLUE   Player: "+result[2]);
         System.out.println("GREEN  Player: "+result[3]);
         return;
    }
}
