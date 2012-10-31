package LUDOSimulator;

public class AniPlayer implements LUDOPlayer {

	LUDOBoard board;

	public AniPlayer(LUDOBoard board){
		this.board = board;
	}
	
	public void play() {
		board.print("AniPlayer playing");
		
		// insert your code here
	}
}

