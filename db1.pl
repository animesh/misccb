#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    Code base of Animesh Sharma [ sharma.animesh@gmail.com ]

use DBI;
my $dsn = "DBI:mysql:database=mydb;host=blrkec03509";#my $dbh = DBI->connect($dsn, 'mysqlu', 'mysqlp',{RaiseError => 1}) ||die $DBI::errstr;
my $dbh = DBI->connect($dsn,'mysqlu', 'mysqlp') or die "Couldn't connect to database: " . DBI->errstr;#my $dbh = DBI->connect('DBI:Oracle:payroll', 'username', 'password')                or die "Couldn't connect to database: " . DBI->errstr;
my $sth = $dbh->prepare(qq{ SELECT user, name, email FROM users_table });
$sth->execute  || die "Error fetching data: $DBI::errstr";
while (my ($user, $name, $email) = $sth->fetchrow_array) {
    print qq{Uzer No.-$user Name-$name may be reached at: $email \n};
}
$dbh->disconnect;

