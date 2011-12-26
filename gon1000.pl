/* $Id: gon1000.pl,v 0.1alpha 2007/04/16 15:36:44 Zzz Exp $

    Part of BioProlog Logic Programming Resources for Bioinformatics

    Author:	Mauro Di Nuzzo
    E-Mail:	info@prologonlinereference.org
    WWW:	http://www.prologonlinereference.org/bioprolog.psp
    Copyright:	�2005-2007 Prolog Online Reference

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/
substitution_matrix(gon1000, [
	('C', 'C', 0.9),
	('C', 'S', 0.0),
	('C', 'T', 0.0),
	('C', 'P', -0.1),
	('C', 'A', 0.0),
	('C', 'G', -0.1),
	('C', 'N', -0.1),
	('C', 'D', -0.1),
	('C', 'E', -0.1),
	('C', 'Q', -0.1),
	('C', 'H', 0.0),
	('C', 'R', -0.1),
	('C', 'K', -0.1),
	('C', 'M', 0.0),
	('C', 'I', 0.0),
	('C', 'L', 0.0),
	('C', 'V', 0.1),
	('C', 'F', 0.1),
	('C', 'Y', 0.1),
	('C', 'W', 0.1),
	('S', 'C', 0.0),
	('S', 'S', 0.0),
	('S', 'T', 0.0),
	('S', 'P', 0.1),
	('S', 'A', 0.0),
	('S', 'G', 0.1),
	('S', 'N', 0.1),
	('S', 'D', 0.1),
	('S', 'E', 0.1),
	('S', 'Q', 0.0),
	('S', 'H', 0.0),
	('S', 'R', 0.0),
	('S', 'K', 0.0),
	('S', 'M', -0.1),
	('S', 'I', -0.1),
	('S', 'L', -0.1),
	('S', 'V', -0.1),
	('S', 'F', -0.2),
	('S', 'Y', -0.1),
	('S', 'W', -0.2),
	('T', 'C', 0.0),
	('T', 'S', 0.0),
	('T', 'T', 0.0),
	('T', 'P', 0.0),
	('T', 'A', 0.0),
	('T', 'G', 0.0),
	('T', 'N', 0.0),
	('T', 'D', 0.0),
	('T', 'E', 0.0),
	('T', 'Q', 0.0),
	('T', 'H', 0.0),
	('T', 'R', 0.0),
	('T', 'K', 0.0),
	('T', 'M', 0.0),
	('T', 'I', 0.0),
	('T', 'L', 0.0),
	('T', 'V', 0.0),
	('T', 'F', -0.1),
	('T', 'Y', -0.1),
	('T', 'W', -0.2),
	('P', 'C', -0.1),
	('P', 'S', 0.1),
	('P', 'T', 0.0),
	('P', 'P', 0.3),
	('P', 'A', 0.0),
	('P', 'G', 0.1),
	('P', 'N', 0.1),
	('P', 'D', 0.1),
	('P', 'E', 0.1),
	('P', 'Q', 0.0),
	('P', 'H', 0.0),
	('P', 'R', 0.0),
	('P', 'K', 0.1),
	('P', 'M', -0.1),
	('P', 'I', -0.1),
	('P', 'L', -0.1),
	('P', 'V', -0.1),
	('P', 'F', -0.2),
	('P', 'Y', -0.2),
	('P', 'W', -0.4),
	('A', 'C', 0.0),
	('A', 'S', 0.0),
	('A', 'T', 0.0),
	('A', 'P', 0.0),
	('A', 'A', 0.0),
	('A', 'G', 0.1),
	('A', 'N', 0.0),
	('A', 'D', 0.0),
	('A', 'E', 0.0),
	('A', 'Q', 0.0),
	('A', 'H', 0.0),
	('A', 'R', 0.0),
	('A', 'K', 0.0),
	('A', 'M', 0.0),
	('A', 'I', 0.0),
	('A', 'L', -0.1),
	('A', 'V', 0.0),
	('A', 'F', -0.1),
	('A', 'Y', -0.1),
	('A', 'W', -0.2),
	('G', 'C', -0.1),
	('G', 'S', 0.1),
	('G', 'T', 0.0),
	('G', 'P', 0.1),
	('G', 'A', 0.1),
	('G', 'G', 0.5),
	('G', 'N', 0.1),
	('G', 'D', 0.2),
	('G', 'E', 0.1),
	('G', 'Q', 0.1),
	('G', 'H', 0.0),
	('G', 'R', 0.1),
	('G', 'K', 0.1),
	('G', 'M', -0.2),
	('G', 'I', -0.3),
	('G', 'L', -0.3),
	('G', 'V', -0.2),
	('G', 'F', -0.4),
	('G', 'Y', -0.3),
	('G', 'W', -0.5),
	('N', 'C', -0.1),
	('N', 'S', 0.1),
	('N', 'T', 0.0),
	('N', 'P', 0.1),
	('N', 'A', 0.0),
	('N', 'G', 0.1),
	('N', 'N', 0.1),
	('N', 'D', 0.1),
	('N', 'E', 0.1),
	('N', 'Q', 0.1),
	('N', 'H', 0.0),
	('N', 'R', 0.1),
	('N', 'K', 0.1),
	('N', 'M', -0.1),
	('N', 'I', -0.1),
	('N', 'L', -0.1),
	('N', 'V', -0.1),
	('N', 'F', -0.2),
	('N', 'Y', -0.1),
	('N', 'W', -0.3),
	('D', 'C', -0.1),
	('D', 'S', 0.1),
	('D', 'T', 0.0),
	('D', 'P', 0.1),
	('D', 'A', 0.0),
	('D', 'G', 0.2),
	('D', 'N', 0.1),
	('D', 'D', 0.2),
	('D', 'E', 0.1),
	('D', 'Q', 0.1),
	('D', 'H', 0.0),
	('D', 'R', 0.1),
	('D', 'K', 0.1),
	('D', 'M', -0.2),
	('D', 'I', -0.2),
	('D', 'L', -0.2),
	('D', 'V', -0.1),
	('D', 'F', -0.3),
	('D', 'Y', -0.2),
	('D', 'W', -0.4),
	('E', 'C', -0.1),
	('E', 'S', 0.1),
	('E', 'T', 0.0),
	('E', 'P', 0.1),
	('E', 'A', 0.0),
	('E', 'G', 0.1),
	('E', 'N', 0.1),
	('E', 'D', 0.1),
	('E', 'E', 0.1),
	('E', 'Q', 0.1),
	('E', 'H', 0.0),
	('E', 'R', 0.1),
	('E', 'K', 0.1),
	('E', 'M', -0.1),
	('E', 'I', -0.1),
	('E', 'L', -0.2),
	('E', 'V', -0.1),
	('E', 'F', -0.2),
	('E', 'Y', -0.2),
	('E', 'W', -0.3),
	('Q', 'C', -0.1),
	('Q', 'S', 0.0),
	('Q', 'T', 0.0),
	('Q', 'P', 0.0),
	('Q', 'A', 0.0),
	('Q', 'G', 0.1),
	('Q', 'N', 0.1),
	('Q', 'D', 0.1),
	('Q', 'E', 0.1),
	('Q', 'Q', 0.1),
	('Q', 'H', 0.0),
	('Q', 'R', 0.1),
	('Q', 'K', 0.1),
	('Q', 'M', -0.1),
	('Q', 'I', -0.1),
	('Q', 'L', -0.1),
	('Q', 'V', -0.1),
	('Q', 'F', -0.1),
	('Q', 'Y', -0.1),
	('Q', 'W', -0.2),
	('H', 'C', 0.0),
	('H', 'S', 0.0),
	('H', 'T', 0.0),
	('H', 'P', 0.0),
	('H', 'A', 0.0),
	('H', 'G', 0.0),
	('H', 'N', 0.0),
	('H', 'D', 0.0),
	('H', 'E', 0.0),
	('H', 'Q', 0.0),
	('H', 'H', 0.0),
	('H', 'R', 0.0),
	('H', 'K', 0.0),
	('H', 'M', 0.0),
	('H', 'I', -0.1),
	('H', 'L', -0.1),
	('H', 'V', 0.0),
	('H', 'F', 0.0),
	('H', 'Y', 0.0),
	('H', 'W', 0.0),
	('R', 'C', -0.1),
	('R', 'S', 0.0),
	('R', 'T', 0.0),
	('R', 'P', 0.0),
	('R', 'A', 0.0),
	('R', 'G', 0.1),
	('R', 'N', 0.1),
	('R', 'D', 0.1),
	('R', 'E', 0.1),
	('R', 'Q', 0.1),
	('R', 'H', 0.0),
	('R', 'R', 0.1),
	('R', 'K', 0.1),
	('R', 'M', -0.1),
	('R', 'I', -0.1),
	('R', 'L', -0.1),
	('R', 'V', -0.1),
	('R', 'F', -0.2),
	('R', 'Y', -0.1),
	('R', 'W', -0.2),
	('K', 'C', -0.1),
	('K', 'S', 0.0),
	('K', 'T', 0.0),
	('K', 'P', 0.1),
	('K', 'A', 0.0),
	('K', 'G', 0.1),
	('K', 'N', 0.1),
	('K', 'D', 0.1),
	('K', 'E', 0.1),
	('K', 'Q', 0.1),
	('K', 'H', 0.0),
	('K', 'R', 0.1),
	('K', 'K', 0.1),
	('K', 'M', -0.1),
	('K', 'I', -0.1),
	('K', 'L', -0.1),
	('K', 'V', -0.1),
	('K', 'F', -0.2),
	('K', 'Y', -0.1),
	('K', 'W', -0.3),
	('M', 'C', 0.0),
	('M', 'S', -0.1),
	('M', 'T', 0.0),
	('M', 'P', -0.1),
	('M', 'A', 0.0),
	('M', 'G', -0.2),
	('M', 'N', -0.1),
	('M', 'D', -0.2),
	('M', 'E', -0.1),
	('M', 'Q', -0.1),
	('M', 'H', 0.0),
	('M', 'R', -0.1),
	('M', 'K', -0.1),
	('M', 'M', 0.2),
	('M', 'I', 0.2),
	('M', 'L', 0.2),
	('M', 'V', 0.1),
	('M', 'F', 0.2),
	('M', 'Y', 0.2),
	('M', 'W', 0.2),
	('I', 'C', 0.0),
	('I', 'S', -0.1),
	('I', 'T', 0.0),
	('I', 'P', -0.1),
	('I', 'A', 0.0),
	('I', 'G', -0.3),
	('I', 'N', -0.1),
	('I', 'D', -0.2),
	('I', 'E', -0.1),
	('I', 'Q', -0.1),
	('I', 'H', -0.1),
	('I', 'R', -0.1),
	('I', 'K', -0.1),
	('I', 'M', 0.2),
	('I', 'I', 0.2),
	('I', 'L', 0.3),
	('I', 'V', 0.2),
	('I', 'F', 0.3),
	('I', 'Y', 0.2),
	('I', 'W', 0.2),
	('L', 'C', 0.0),
	('L', 'S', -0.1),
	('L', 'T', 0.0),
	('L', 'P', -0.1),
	('L', 'A', -0.1),
	('L', 'G', -0.3),
	('L', 'N', -0.1),
	('L', 'D', -0.2),
	('L', 'E', -0.2),
	('L', 'Q', -0.1),
	('L', 'H', -0.1),
	('L', 'R', -0.1),
	('L', 'K', -0.1),
	('L', 'M', 0.2),
	('L', 'I', 0.3),
	('L', 'L', 0.3),
	('L', 'V', 0.2),
	('L', 'F', 0.3),
	('L', 'Y', 0.2),
	('L', 'W', 0.2),
	('V', 'C', 0.1),
	('V', 'S', -0.1),
	('V', 'T', 0.0),
	('V', 'P', -0.1),
	('V', 'A', 0.0),
	('V', 'G', -0.2),
	('V', 'N', -0.1),
	('V', 'D', -0.1),
	('V', 'E', -0.1),
	('V', 'Q', -0.1),
	('V', 'H', 0.0),
	('V', 'R', -0.1),
	('V', 'K', -0.1),
	('V', 'M', 0.1),
	('V', 'I', 0.2),
	('V', 'L', 0.2),
	('V', 'V', 0.1),
	('V', 'F', 0.2),
	('V', 'Y', 0.1),
	('V', 'W', 0.1),
	('F', 'C', 0.1),
	('F', 'S', -0.2),
	('F', 'T', -0.1),
	('F', 'P', -0.2),
	('F', 'A', -0.1),
	('F', 'G', -0.4),
	('F', 'N', -0.2),
	('F', 'D', -0.3),
	('F', 'E', -0.2),
	('F', 'Q', -0.1),
	('F', 'H', 0.0),
	('F', 'R', -0.2),
	('F', 'K', -0.2),
	('F', 'M', 0.2),
	('F', 'I', 0.3),
	('F', 'L', 0.3),
	('F', 'V', 0.2),
	('F', 'F', 0.6),
	('F', 'Y', 0.5),
	('F', 'W', 0.9),
	('Y', 'C', 0.1),
	('Y', 'S', -0.1),
	('Y', 'T', -0.1),
	('Y', 'P', -0.2),
	('Y', 'A', -0.1),
	('Y', 'G', -0.3),
	('Y', 'N', -0.1),
	('Y', 'D', -0.2),
	('Y', 'E', -0.2),
	('Y', 'Q', -0.1),
	('Y', 'H', 0.0),
	('Y', 'R', -0.1),
	('Y', 'K', -0.1),
	('Y', 'M', 0.2),
	('Y', 'I', 0.2),
	('Y', 'L', 0.2),
	('Y', 'V', 0.1),
	('Y', 'F', 0.5),
	('Y', 'Y', 0.5),
	('Y', 'W', 0.9),
	('W', 'C', 0.1),
	('W', 'S', -0.2),
	('W', 'T', -0.2),
	('W', 'P', -0.4),
	('W', 'A', -0.2),
	('W', 'G', -0.5),
	('W', 'N', -0.3),
	('W', 'D', -0.4),
	('W', 'E', -0.3),
	('W', 'Q', -0.2),
	('W', 'H', 0.0),
	('W', 'R', -0.2),
	('W', 'K', -0.3),
	('W', 'M', 0.2),
	('W', 'I', 0.2),
	('W', 'L', 0.2),
	('W', 'V', 0.1),
	('W', 'F', 0.9),
	('W', 'Y', 0.9),
	('W', 'W', 3.2)
]).
