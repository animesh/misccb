package LUDOSimulator;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Random;

public class AnIPlay implements LUDOPlayer {

	LUDOBoard board;
	Random rand;
	int playCnter;
	//File f = new File("filename");
	//boolean success =f.delete();
	public AnIPlay(LUDOBoard board){
		this.board = board;
		rand = new Random();
		playCnter = 0;
	}
	
	
	public void play() {
		playCnter++;
		if(playCnter%100==0){		
			BufferedWriter out;
				try {
					out = new BufferedWriter(new FileWriter("filename", true));
					out.write("Random:\t"+rand.nextInt(100)+"\tAnIPlay counter:\t"+playCnter+"\n"); 
					out.close(); 		
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} 
		}
		board.print("AnIPlay");
		board.rollDice();
		float nrmx =-1;
		int nrbest = -1;
		for(int i=0;i<4;i++)
		{
			float value = analyzeBrickSituation(i); 
			if(value>nrmx&&value>0) {
				nrbest = i;
				nrmx = value;
			}
		}
		if(nrbest!=-1) board.moveBrick(nrbest);
	}
	public float analyzeBrickSituation(int i) {
		if(board.moveable(i)) {
			int[][] current_board = board.getBoardState();
			int[][] new_board = board.getNewBoardState(i, board.getMyColor(), board.getDice());
			
			if(hitOpponentHome(current_board,new_board)) {
				return 5+rand.nextFloat();
			}
			else if(hitMySelfHome(current_board,new_board)) {
				return (float)0.1;
			}
			else if(board.isStar(new_board[board.getMyColor()][i])) {
				return 4+rand.nextFloat();
			}
			else if(moveOut(current_board,new_board)) {
				return 3+rand.nextFloat();
			}
			else if(board.atHome(new_board[board.getMyColor()][i],board.getMyColor())) {
				return 2+rand.nextFloat();
			}
			else {
				return 1+rand.nextFloat();
			}
		}
		else {
			return 0;
		}
	}


	private boolean moveOut(int[][] current_board, int[][] new_board) {
		for(int i=0;i<4;i++) {
			if(board.inStartArea(current_board[board.getMyColor()][i],board.getMyColor())&&!board.inStartArea(new_board[board.getMyColor()][i],board.getMyColor())) {
				return true;
			}
		}
		return false;
	}

	private boolean hitOpponentHome(int[][] current_board, int[][] new_board) {
		for(int i=0;i<4;i++) {
			for(int j=0;j<4;j++) {
				if(board.getMyColor()!=i) {
					if(board.atField(current_board[i][j])&&!board.atField(new_board[i][j])) {
						return true;
					}
				}
			}
		}
		return false;
	}
	private boolean hitMySelfHome(int[][] current_board, int[][] new_board) {
		for(int i=0;i<4;i++) {
			if(!board.inStartArea(current_board[board.getMyColor()][i],board.getMyColor())&&board.inStartArea(new_board[board.getMyColor()][i],board.getMyColor())) {
				return true;
			}
		}
		return false;
	}
}

